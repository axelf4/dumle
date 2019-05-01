//! Implementations of [React-style hooks](https://reactjs.org/docs/hooks-overview.html).
//!
//! Hooks are virtual nodes that let you *hook into* lifecycle features.

use crate::{Context, Cursor, SingleNode, Vnode};
use std::cell::{Cell, RefCell};
use std::rc::{Rc, Weak};
use std::{default::Default, fmt, mem};
use wasm_bindgen::UnwrapThrowExt;
use web_sys::Node;

struct UseStateData<N, S, R> {
    /// The virtual node.
    vnode: N,
    /// The mounted DOM node.
    node: Node,
    /// User data.
    user: S,
    /// The render function.
    render: R,
}

enum UseStatePhase<N, S, R> {
    Unmounted(R),
    Live(Rc<RefCell<Option<UseStateData<N, S, R>>>>),
}

/// Stateful virtual node that renders virtual trees returned by a given function.
///
/// The render function is given a reference to the current state and
/// a function for updating the state, which would prompt a re-render.
///
/// ## Example
///
/// The following constructs a virtual node that renders a simple counter:
///
/// ```no_run
/// use dumle::{html, hook::UseState};
/// let node = UseState::new(|state: &i32, set_state| {
///     html! {
///         <div>
///             { state.to_string() }
///             <button click=move |_| set_state(&|state: &mut i32| *state += 1),>
///                 {"Increment"}
///             </button>
///         </div>
///     }
/// });
/// ```
pub struct UseState<N, S, R>(Cell<UseStatePhase<N, S, R>>);

impl<N: SingleNode, S, R> UseState<N, S, R>
where
    R: FnMut(&S, Rc<dyn Fn(&Fn(&mut S))>) -> N + 'static,
{
    /// Returns a new unmounted [UseState] with the specified render function.
    pub fn new(render: R) -> Self {
        UseState(Cell::new(UseStatePhase::Unmounted(render)))
    }
}

fn patch_new_state<N: SingleNode + 'static, S: 'static, R>(
    new_state: &Fn(&mut S),
    state: &Weak<RefCell<Option<UseStateData<N, S, R>>>>,
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
            (Some(UseState(state)), None) => {
                if let UseStatePhase::Live(state) = state.into_inner() {
                    let mut state = state.borrow_mut();
                    if let Some(state) = state.take() {
                        N::patch(ctx, Some(state.vnode), None);
                    } else {
                        unreachable!()
                    }
                } else {
                    unreachable!()
                }
            }
            _ => unreachable!(),
        }
    }
}

impl<N, S, R> fmt::Debug for UseState<N, S, R> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "UseState")
    }
}

impl<N: fmt::Display, S: Default, R> fmt::Display for UseState<N, S, R>
where
    R: FnMut(&S, Rc<dyn Fn(&Fn(&mut S))>) -> N + 'static,
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let fake_live = Rc::new(RefCell::new(None));
        if let UseStatePhase::Unmounted(mut render) = self.0.replace(UseStatePhase::Live(fake_live))
        {
            let state = Default::default();
            let update = Rc::new(|_set_state: &Fn(&mut S)| {});
            let result = render(&state, update).fmt(f);
            self.0.set(UseStatePhase::Unmounted(render));
            result
        } else {
            unimplemented!()
        }
    }
}
