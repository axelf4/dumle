use lis::{diff_by_key, DiffCallback};
use std::cell::Cell;
use std::hash::Hash;
use std::mem;
use wasm_bindgen::JsCast;
use web_sys::{Document, Event, Node};

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
pub trait SingleNode: Vnode {}

/// Marker trait for virtual nodes that correspond to a single DOM element.
pub trait SingleElement: SingleNode {}

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
                // TODO Parent should take care of recursively removing children
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
                #[derive(PartialEq, Eq, Debug)]
                pub struct $name;
                impl Element for $name {
                    const NAME: &'static str = stringify!($name);
                }
            )+
        }
    }

    impl_tags! {div, button}
}

#[derive(Debug)]
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

#[derive(Debug)]
pub struct Child<P, C>(pub P, pub C);

impl<P: SingleNode, C: Vnode> Vnode for Child<P, C> {
    fn patch(ctx: &mut Context, p: Option<Self>, n: Option<&Self>) {
        let (pa, pb) = p
            .map(|Child(a, b)| (Some(a), Some(b)))
            .unwrap_or((None, None));
        let (na, nb) = n
            .map(|Child(a, b)| (Some(a), Some(b)))
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

impl<P: SingleNode, C: Vnode> SingleNode for Child<P, C> {}
impl<P: SingleElement, C: Vnode> SingleElement for Child<P, C> {}

#[doc(hidden)]
#[macro_export]
macro_rules! local_stringify {
    ($s:ident) => {
        stringify!($s)
    };
}

/// Separate implementation to prevent infinite recursion.
#[doc(hidden)]
#[macro_export(local_inner_macros)]
macro_rules! html_impl {
    // Start of opening tag
    ($($sibling:ident)? ($($stack:tt)*) < $tag:ident $($tt:tt)+) => {
        let node = $crate::tags::$tag;
        html_impl!{@tag (node:$($sibling)?, $($stack)*) $($tt)+}
    };
    // End of opening tag
    (@tag ($($stack:tt)*) > $($tt:tt)+) => {html_impl!{($($stack)*) $($tt)+}};
    // Self-closing tag
    (@tag ($node:ident:$($sibling:ident)?, $($stack:tt)*) /> $($tt:tt)*) => {
        $(let $node = ($sibling, $node);)? // If had siblings
        html_impl!{$node ($($stack)*) $($tt)*}
    };
    // Attribute
    (@tag ($node:ident $($stack:tt)*) $attr:ident = $val:expr, $($tt:tt)+) => {
        let $node = $crate::ToAttribute::to_attribute(($val, $node), local_stringify!($attr));
        html_impl!{@tag ($node $($stack)*) $($tt)*}
    };
    // Expression block
    ($($sibling:ident)? ($($stack:tt)*) { $eval:expr } $($tt:tt)*) => {
        let node = $crate::ToVnode::to_vnode($eval);
        $(let node = ($sibling, node);)? // If had siblings
        html_impl!{node ($($stack)*) $($tt)*}
    };
    // End tag
    ($($child:ident)? ($node:ident:$($sibling:ident)?, $($stack:tt)*) </ $tag:ident > $($tt:tt)*) => {
        $(let $node = $crate::Child($node, $child);)? // If had child
        $(let $node = ($sibling, $node);)? // If had siblings
        html_impl!{$node ($($stack)*) $($tt)*}
    };
    ($sibling:ident ()) => {$sibling};
}

/// A convenience macro for building trees with a HTML-esque language.
#[macro_export]
macro_rules! html {
    ($($tt:tt)+) => {{$crate::html_impl!{() $($tt)+}}};
}

// TODO Parameter for type of value
/// An attribute on a DOM node.
#[derive(Debug)]
pub struct Attribute<N> {
    // Const-generics help here. Could use trait until const-generics arrive, but meh
    /// The name of the DOM attribute.
    name: &'static str,
    /// The current value of this attribute.
    value: &'static str,
    /// The virtual node to set the attribute on.
    node: N,
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

impl<N: SingleElement> SingleNode for Attribute<N> {}
impl<N: SingleElement> SingleElement for Attribute<N> {}

// Note that the callback can never change due to the type being constant
pub struct Listener<N, F> {
    // TODO Make constant with const-generics
    event: &'static str,
    callback: Cell<Option<F>>,
    closure: Cell<Option<Closure<FnMut(Event)>>>,
    node: N,
}

impl<N: SingleNode, F: FnMut(Event) + 'static> Vnode for Listener<N, F> {
    fn patch(ctx: &mut Context, p: Option<Self>, n: Option<&Self>) {
        let (pa, pb) = p
            .map(
                |Listener {
                     event,
                     callback,
                     closure,
                     node,
                 }| (Some((event, closure)), Some(node)),
            )
            .unwrap_or((None, None));
        let (na, nb) = n
            .map(
                |Listener {
                     event,
                     callback,
                     closure,
                     node,
                 }| (Some((event, callback, closure)), Some(node)),
            )
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
            (Some(_), None) => {}
            (None, Some((name, callback, closure))) => {
                let callback = callback.take().unwrap();
                let cb = Closure::wrap(Box::new(callback) as Box<dyn FnMut(Event)>);
                element.add_event_listener_with_callback("click", cb.as_ref().unchecked_ref());
                closure.set(Some(cb));
            }
            (Some((pevent, pclosure)), Some((nevent, _, nclosure))) => {
                if &pevent != nevent {
                    unimplemented!()
                }
                pclosure.swap(nclosure)
            }
            _ => unreachable!(),
        }
    }
}

impl<N: SingleNode, F: FnMut(Event) + 'static> SingleNode for Listener<N, F> {}
impl<N: SingleElement, F: FnMut(Event) + 'static> SingleElement for Listener<N, F> {}

impl<N: SingleNode, F: FnMut(Event) + 'static> Listener<N, F> {
    pub fn unattached(node: N, event: &'static str, callback: F) -> Self {
        Listener {
            event,
            callback: Cell::new(Some(callback)),
            closure: Cell::new(None),
            node,
        }
    }
}

pub trait ToAttribute<N: Vnode> {
    type Output: Vnode;
    fn to_attribute(self, name: &'static str) -> Self::Output;
}

impl<N: SingleElement> ToAttribute<N> for (&'static str, N) {
    type Output = Attribute<N>;
    fn to_attribute(self, name: &'static str) -> Self::Output {
        Attribute {
            name,
            value: self.0,
            node: self.1,
        }
    }
}

impl<N: SingleNode, F: FnMut(Event) + 'static> ToAttribute<N> for (F, N) {
    type Output = Listener<N, F>;
    fn to_attribute(self, event: &'static str) -> Self::Output {
        Listener::unattached(self.1, event, self.0)
    }
}

pub trait ToVnode {
    type Output: Vnode;
    fn to_vnode(self) -> Self::Output;
}

impl<T: Vnode> ToVnode for T {
    type Output = Self;
    fn to_vnode(self) -> Self::Output {
        self
    }
}

impl ToVnode for &'static str {
    type Output = Text<Self>;
    fn to_vnode(self) -> Self::Output {
        Text(self)
    }
}

impl ToVnode for String {
    type Output = Text<Self>;
    fn to_vnode(self) -> Self::Output {
        Text(self)
    }
}

/// A value with an associated key.
pub struct Keyed<K, T> {
    key: K,
    value: T,
}

// TODO store DOM nodes
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

pub struct Hook<T, R: Fn(&T, Fn(T)) -> SingleNode> {
    state: RefCell<T>,
    render: R,
    node: Node,
}

#[cfg(test)]
mod tests {
    use super::{html, tags::div};

    #[test]
    fn macro_self_closing_tag() {
        assert_eq!(html! {<div />}, div)
    }
}
