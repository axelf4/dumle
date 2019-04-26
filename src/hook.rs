use crate::{Context, Cursor, SingleNode, Vnode};
use std::cell::{Cell, RefCell};
use std::default::Default;
use std::mem;
use std::rc::Rc;
use wasm_bindgen::UnwrapThrowExt;
use web_sys::Node;

struct UseStateData<N, S, R> {
    vnode: N,
    node: Node,
    /// User data.
    user: S,
    render: R,
}

enum UseStatePhase<N, S, R> {
    Unmounted(R),
    Live(Rc<RefCell<Option<UseStateData<N, S, R>>>>),
}

pub struct UseState<N, S, R>(Cell<UseStatePhase<N, S, R>>);

impl<N: SingleNode, S, R> UseState<N, S, R>
where
    R: FnMut(&S, Rc<dyn Fn(&Fn(&mut S))>) -> N + 'static,
{
    pub fn new(render: R) -> Self {
        UseState(Cell::new(UseStatePhase::Unmounted(render)))
    }
}

fn patch_new_state<N: SingleNode + 'static, S: 'static, R>(
    new_state: &Fn(&mut S),
    state: &std::rc::Weak<RefCell<Option<UseStateData<N, S, R>>>>,
) where
    R: FnMut(&S, Rc<dyn Fn(&Fn(&mut S))>) -> N + 'static,
{
    let state = state.upgrade().expect_throw("State should exist");

    let update = {
        let state = Rc::downgrade(&state);
        // Parameter type required because of https://github.com/rust-lang/rust/issues/41078
        Rc::new(move |new_state: &Fn(&mut S)| patch_new_state(new_state, &state))
    };

    let mut state = state.borrow_mut();
    let mut state = state.as_mut().unwrap_throw();

    new_state(&mut state.user);
    // Render new vnode
    let pvnode = mem::replace(&mut state.vnode, (state.render)(&state.user, update));

    // Reconcile the DOM
    let mut ctx = Context {
        cursor: Cursor {
            parent: state.node.parent_node().unwrap_throw(),
            child: state.node.next_sibling(),
        },
    };
    N::patch(&mut ctx, Some(pvnode), Some(&state.vnode));
    // Store potentially new DOM node
    state.node = ctx.cursor.child.expect_throw("Should point at node");
}

impl<N: SingleNode + 'static, S: Default + 'static, R> Vnode for UseState<N, S, R>
where
    R: FnMut(&S, Rc<dyn Fn(&Fn(&mut S))>) -> N + 'static,
{
    fn patch(ctx: &mut Context, p: Option<Self>, n: Option<&Self>) {
        match (p, n) {
            (None, Some(UseState(phase))) => {
                let state = Rc::new(RefCell::new(None));
                let user = Default::default();
                let update = {
                    let state = Rc::downgrade(&state);
                    Rc::new(move |new_state: &Fn(&mut S)| patch_new_state(new_state, &state))
                };

                if let UseStatePhase::Unmounted(mut render) =
                    phase.replace(UseStatePhase::Live(state.clone()))
                {
                    let vnode = render(&user, update);
                    N::patch(ctx, None, Some(&vnode));
                    let node = ctx
                        .cursor
                        .child
                        .clone()
                        .expect_throw("Should point at node");

                    state.replace(Some(UseStateData {
                        vnode,
                        node,
                        user,
                        render,
                    }));
                } else {
                    unreachable!()
                }
            }
            (Some(UseState(ref pstate)), Some(UseState(nstate))) => {
                pstate.swap(nstate);
                // TODO Render new VDOM tree using the NEW render function which
                // could have copied changed variables

                ctx.cursor.child = ctx
                    .cursor
                    .child
                    .as_ref()
                    .map_or_else(|| ctx.cursor.parent.last_child(), Node::previous_sibling);
                debug_assert!(ctx.cursor.child.is_some(), "This node is not found");
            }
            (Some(UseState(_state)), None) => { /* TODO */ }
            _ => unreachable!(),
        }
    }
}
