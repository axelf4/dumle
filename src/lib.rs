use lis::{diff_by_key, DiffCallback};
use std::hash::Hash;
use std::mem;
use wasm_bindgen::JsCast;
use web_sys::{Document, Node};

use wasm_bindgen::prelude::*;

#[wasm_bindgen]
extern "C" {
    // Use `js_namespace` here to bind `console.log(..)` instead of just
    // `log(..)`
    #[wasm_bindgen(js_namespace = console)]
    fn log(s: &str);
}

macro_rules! console_log {
    ($($t:tt)*) => (log(&format_args!($($t)*).to_string()))
}

#[derive(Clone, Debug)]
pub enum Cursor {
    /// Points at the parent. Behave as if right sibling doesn't exist.
    Parent(Node),
    /// Points at the previous sibling.
    Child(Node),
}

/// Context during rendering.
pub struct Context {
    /// The last touched DOM node and its relation to the next node.
    cursor: Cursor,
}

impl Context {
    pub fn from(node: Node) -> Context {
        Context {
            cursor: Cursor::Parent(node),
        }
    }

    pub fn patch<T: Vnode>(mut self, p: Option<T>, n: Option<&T>) {
        T::patch(&mut self, p, n)
    }
}

fn document() -> Document {
    web_sys::window().expect("").document().expect("")
}

pub trait Vnode {
    // TODO want check for some/none to be done at compile time
    fn patch(ctx: &mut Context, p: Option<Self>, n: Option<&Self>)
    where
        Self: Sized;
}

/// Marker trait for virtual nodes that correspond to a single DOM node.
trait SingleNode: Vnode {}

/// Marker trait for virtual nodes that correspond to a single DOM element.
trait SingleElement: SingleNode {}

// TODO Add trait for constant nodes
// And skip updating all children if all children are constant

/// A virtual DOM element with a given name.
pub trait Element {
    /// The name of the DOM element.
    const NAME: &'static str;
}

impl<E: Element> SingleNode for E {}
impl<E: Element> SingleElement for E {}

impl<E: Element> Vnode for E {
    fn patch(ctx: &mut Context, p: Option<Self>, n: Option<&Self>) {
        match (p, n) {
            (None, Some(_)) => {
                let element = document()
                    .create_element(Self::NAME)
                    .expect("Failed to create element");

                match ctx.cursor {
                    Cursor::Parent(ref parent) => parent.append_child(&element),
                    Cursor::Child(ref sibling) => sibling
                        .parent_node()
                        .expect("No parent")
                        .insert_before(&element, Some(sibling)),
                }
                .expect("Failed to insert child");

                ctx.cursor = Cursor::Child(element.into());
            }
            (Some(_), Some(_)) => {
                // Update cursor
                let node = match ctx.cursor {
                    Cursor::Parent(ref parent) => parent.last_child(),
                    Cursor::Child(ref sibling) => sibling.previous_sibling(),
                }
                .expect("This element is not found");
                ctx.cursor = Cursor::Child(node);
                // Updating is no-op
            }
            (Some(_), None) => {
                match ctx.cursor {
                    Cursor::Parent(ref parent) => parent
                        .remove_child(&parent.last_child().expect("This element is not found")),
                    Cursor::Child(ref sibling) => {
                        sibling.parent_node().expect("No parent node").remove_child(
                            &sibling
                                .previous_sibling()
                                .expect("This element is not found"),
                        )
                    }
                }
                .expect("Failed to remove node");
            }
            _ => unreachable!(),
        }
    }
}

#[allow(non_camel_case_types)]
pub mod tags {
    use super::Element;

    macro_rules! impl_tags {
        ($($name:ident),+) => {
            $(
                pub struct $name;
                impl Element for $name {
                    const NAME: &'static str = stringify!($name);
                }
            )+
        }
    }

    impl_tags! {div, button}
}

pub struct Text<T>(pub T);

// TODO Add ZST for constant text using const generics

impl<T: AsRef<str>> Vnode for Text<T> {
    fn patch(ctx: &mut Context, p: Option<Self>, n: Option<&Self>) {
        match (p, n) {
            (None, Some(Text(data))) => {
                let node = document().create_text_node(data.as_ref());

                ctx.cursor = Cursor::Child(
                    match ctx.cursor {
                        Cursor::Parent(ref parent) => parent.append_child(&node),
                        Cursor::Child(ref sibling) => sibling
                            .parent_node()
                            .expect("No parent")
                            .insert_before(&node, Some(sibling)),
                    }
                    .expect("Failed to insert child"),
                );
            }
            (Some(Text(ref old)), Some(Text(new))) => {
                // Update cursor
                let node = match ctx.cursor {
                    Cursor::Parent(ref parent) => parent.last_child(),
                    Cursor::Child(ref sibling) => sibling.previous_sibling(),
                }
                .expect("This element is not found");
                if old.as_ref() != new.as_ref() {
                    node.set_node_value(Some(new.as_ref()));
                }
                ctx.cursor = Cursor::Child(node);
            }
            (Some(_), None) => {
                match ctx.cursor {
                    Cursor::Parent(ref parent) => {
                        parent.remove_child(&parent.last_child().expect("This node is not found"))
                    }
                    Cursor::Child(ref sibling) => sibling
                        .parent_node()
                        .expect("No parent node")
                        .remove_child(&sibling.previous_sibling().expect("This node is not found")),
                }
                .expect("Failed to remove node");
            }
            _ => unreachable!(),
        }
    }
}

impl<T: AsRef<str>> SingleNode for Text<T> {}

pub struct ChildCons<P, C>(pub P, pub C);

impl<P: SingleNode, C: Vnode> Vnode for ChildCons<P, C> {
    fn patch(ctx: &mut Context, p: Option<Self>, n: Option<&Self>) {
        let (pa, pb) = p
            .map(|ChildCons(a, b)| (Some(a), Some(b)))
            .unwrap_or((None, None));
        let (na, nb) = n
            .map(|ChildCons(a, b)| (Some(a), Some(b)))
            .unwrap_or((None, None));
        P::patch(ctx, pa, na);
        // Specify current cursor as parent instead
        let new_cursor = Cursor::Parent(match ctx.cursor {
            Cursor::Child(ref node) => node.clone(),
            Cursor::Parent(_) => unreachable!(),
        });
        let old_cur = mem::replace(&mut ctx.cursor, new_cursor);
        C::patch(ctx, pb, nb);
        ctx.cursor = old_cur;
    }
}

impl<P: SingleNode, C: SingleNode> SingleNode for ChildCons<P, C> {}

pub struct Attribute<N> {
    /// The name of the DOM attribute.
    /// Const-generics help here.
    pub name: &'static str,
    /// The current value of this attribute.
    pub value: &'static str,
    /// The virtual node to set the attribute on.
    pub node: N,
}

impl<N: SingleElement> Vnode for Attribute<N> {
    #[inline(always)]
    fn patch(ctx: &mut Context, p: Option<Self>, n: Option<&Self>) {
        let (pa, pb) = p
            .map(|Attribute { name, value, node }| (Some((name, value)), Some(node)))
            .unwrap_or((None, None));
        let (na, nb) = n
            .map(|Attribute { name, value, node }| (Some((name, value)), Some(node)))
            .unwrap_or((None, None));
        N::patch(ctx, pb, nb);

        // Cannot set attribute for removed element
        if nb.is_none() {
            return;
        }

        let element: &web_sys::Element = match &ctx.cursor {
            Cursor::Child(node) => node,
            Cursor::Parent(_) => unreachable!(),
        }
        .dyn_ref()
        .expect("Failed to get element from cursor");
        match (pa, na) {
            (Some((name, _)), None) => element.remove_attribute(name).unwrap(),
            (None, Some((name, value))) => element.set_attribute(name, value).unwrap(),
            (Some((name, old)), Some((_, new))) => {
                if &old != new {
                    element.set_attribute(name, new).unwrap()
                }
            }
            _ => unreachable!(),
        }
    }
}

// TODO store DOM nodes

/// A value with an associated key.
pub struct Keyed<K, T> {
    key: K,
    value: T,
}

// Allow constructing cons lists
impl<A: Vnode, B: Vnode> Vnode for (A, B) {
    #[inline(always)]
    fn patch(ctx: &mut Context, p: Option<Self>, n: Option<&Self>) {
        let (pa, pb) = p.map(|(a, b)| (Some(a), Some(b))).unwrap_or((None, None));
        let (na, nb) = n.map(|(a, b)| (Some(a), Some(b))).unwrap_or((None, None));
        B::patch(ctx, pb, nb); // XXX: Do B first!
        A::patch(ctx, pa, na);
    }
}

/// Keyed lists
pub struct Dyn<K, T>(Vec<Keyed<K, T>>);

/*impl<K: Eq + Hash, T: SingleNode> Vnode for Dyn<K, T> {
    fn patch(ctx: &mut Context, p: Option<Self>, n: Option<&Self>) {
        let old = p.map(|p| p.0).unwrap_or_default();
        let new = &n.unwrap().0; // TODO

        struct Cb;
        impl<K: Eq + Hash, T: Vnode> DiffCallback<Keyed<K, T>, &Keyed<K, T>> for Cb {
            fn inserted(&mut self, new: &Keyed<K, T>) {}
            fn removed(&mut self, old: Keyed<K, T>) {}
            fn unchanged(&mut self, old: Keyed<K, T>, new: &Keyed<K, T>) {}
        }
        let mut cb = Cb;
        diff_by_key(old.into_iter(), new.iter(), |x| &x.key, |x| &x.key, &mut cb);
    }
}
*/

// Hooks maybe?

use std::cell::RefCell;

pub struct Hook<T, R: Fn(&T, Fn(T)) -> Vnode> {
    state: RefCell<T>,
    // render: Fn(&T, Fn(T)) -> Vnode,
    render: R,
    node: Node,
}
