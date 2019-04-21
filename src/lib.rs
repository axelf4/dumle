use lis::{diff_by_key, DiffCallback};
use std::cell::Cell;
use std::hash::Hash;
use std::marker::PhantomData;
use wasm_bindgen::prelude::*;
use wasm_bindgen::JsCast;
use web_sys::{Document, Event, EventTarget, Node};

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
    fn patch(ctx: &mut Context, p: Option<Self>, n: Option<&Self>)
    where
        Self: Sized;
}

/// Trait for virtual nodes that correspond to a single DOM node.
pub trait SingleNode {
    fn create_node(&self) -> Node;

    fn update(&self, _old: Self, _node: &Node)
    where
        Self: Sized,
    {
    }
}

impl<T: SingleNode> Vnode for T {
    #[inline(always)]
    fn patch(ctx: &mut Context, p: Option<Self>, n: Option<&Self>) {
        match (p, n) {
            (None, Some(n)) => {
                let node = n.create_node();
                match ctx.cursor {
                    Cursor::Parent(ref parent) => parent.append_child(&node),
                    Cursor::Child(ref sibling) => sibling
                        .parent_node()
                        .expect("No parent")
                        .insert_before(&node, Some(sibling)),
                }
                .expect("Failed to insert child");
                ctx.cursor = Cursor::Child(node);
            }
            (Some(p), Some(n)) => {
                let node = match ctx.cursor {
                    Cursor::Parent(ref parent) => parent.last_child(),
                    Cursor::Child(ref sibling) => sibling.previous_sibling(),
                }
                .expect("This node is not found");
                n.update(p, &node);
                ctx.cursor = Cursor::Child(node); // Update cursor
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

// TODO Once specialization lands: skip updating children if all are constant
/// Marker trait for virtual nodes that never need updating once mounted.
#[allow(unused)]
pub trait ConstantNode: Vnode {}

/// Marker trait for virtual nodes that correspond to a single DOM element.
pub trait SingleElement: SingleNode {}

/// A virtual DOM element with a given name.
pub trait Element {
    /// The name of the DOM element.
    const NAME: &'static str;
}

impl<E: Element> SingleNode for E {
    #[inline(always)]
    fn create_node(&self) -> Node {
        document()
            .create_element(Self::NAME)
            .expect("Failed to create element")
            .into()
    }
}

impl<E: Element> SingleElement for E {}

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

impl<T: AsRef<str> + PartialEq> SingleNode for Text<T> {
    #[inline(always)]
    fn create_node(&self) -> Node {
        document().create_text_node(self.0.as_ref()).into()
    }

    #[inline(always)]
    fn update(&self, old: Self, node: &Node) {
        if old.0 != self.0 {
            node.set_node_value(Some(self.0.as_ref()));
        }
    }
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

#[derive(Debug)]
pub struct Child<P, C>(pub P, pub C);

impl<P: SingleNode, C: Vnode> SingleNode for Child<P, C> {
    #[inline(always)]
    fn create_node(&self) -> Node {
        let parent_node = self.0.create_node();
        let mut ctx = Context {
            cursor: Cursor::Parent(parent_node.clone()),
        };
        C::patch(&mut ctx, None, Some(&self.1));
        parent_node
    }
    #[inline(always)]
    fn update(&self, old: Self, node: &Node) {
        self.0.update(old.0, node);
        let mut ctx = Context {
            cursor: Cursor::Parent(node.clone()),
        };
        C::patch(&mut ctx, Some(old.1), Some(&self.1));
    }
}

impl<P: SingleElement, C> SingleElement for Child<P, C> where Child<P, C>: SingleNode {}

/// Separate implementation to prevent infinite recursion.
#[doc(hidden)]
#[macro_export]
macro_rules! html_impl {
    // Start of opening tag
    ($($sibling:ident)? ($($stack:tt)*) < $tag:ident $($tt:tt)+) => {
        let node = $crate::tags::$tag;
        $crate::html_impl!{@tag (node:$tag:$($sibling)?, $($stack)*) $($tt)+}
    };
    // End of opening tag
    (@tag ($($stack:tt)*) > $($tt:tt)+) => {$crate::html_impl!{($($stack)*) $($tt)+}};
    // Self-closing tag
    (@tag ($node:ident:$name:ident:$($sibling:ident)?, $($stack:tt)*) /> $($tt:tt)*) => {
        $(let $node = ($sibling, $node);)? // If had siblings
        $crate::html_impl!{$node ($($stack)*) $($tt)*}
    };
    // Attribute
    (@tag ($node:ident $($stack:tt)*) $attr:ident = $val:expr, $($tt:tt)+) => {
        let $node = $crate::ToAttribute::to_attribute(($val, $node), stringify!($attr));
        $crate::html_impl!{@tag ($node $($stack)*) $($tt)*}
    };
    // Expression block
    ($($sibling:ident)? ($($stack:tt)*) $eval:block $($tt:tt)*) => {
        let node = $crate::ToVnode::to_vnode($eval);
        $(let node = ($sibling, node);)? // If had siblings
        $crate::html_impl!{node ($($stack)*) $($tt)*}
    };
    // End tag
    ($($child:ident)? ($node:ident:$name:ident:$($sibling:ident)?, $($stack:tt)*) </ $tag:ident > $($tt:tt)*) => {
        assert_eq!(stringify!($name), stringify!($tag), "Mismatched tag");
        $(let $node = $crate::Child($node, $child);)? // If had child
        $(let $node = ($sibling, $node);)? // If had siblings
        $crate::html_impl!{$node ($($stack)*) $($tt)*}
    };
    ($sibling:ident ()) => {$sibling};
}

/// A convenience macro for building trees with a HTML-esque language.
#[macro_export]
macro_rules! html {
    ($($tt:tt)+) => {{$crate::html_impl!{() $($tt)+}}};
}

// TODO Parameter for type of value
// TODO Add toggle-able attributes using trait
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

impl<N: SingleElement> SingleNode for Attribute<N> {
    #[inline(always)]
    fn create_node(&self) -> Node {
        let node = self.node.create_node();
        node.unchecked_ref::<web_sys::Element>()
            .set_attribute(self.name, self.value)
            .expect("Failed to set attribute");
        node
    }

    #[inline(always)]
    fn update(&self, old: Self, node: &Node) {
        self.node.update(old.node, node);
        if self.value != old.value {
            node.unchecked_ref::<web_sys::Element>()
                .set_attribute(self.name, self.value)
                .expect("Failed to set attribute");
        }
    }
}

impl<N: SingleElement> SingleElement for Attribute<N> {}

pub struct Listener<N, F> {
    // TODO Make constant with const-generics
    event: &'static str,
    callback: Cell<Option<F>>,
    closure: Cell<Option<Closure<FnMut(Event)>>>,
    node: N,
}

impl<N: SingleNode, F: FnMut(Event) + 'static> SingleNode for Listener<N, F> {
    fn create_node(&self) -> Node {
        let node = self.node.create_node();
        let cb = self.callback.take().unwrap();
        let cb = Closure::wrap(Box::new(cb) as Box<dyn FnMut(Event)>);
        node.unchecked_ref::<EventTarget>()
            .add_event_listener_with_callback(self.event, cb.as_ref().unchecked_ref())
            .unwrap();
        self.closure.set(Some(cb));
        node
    }

    fn update(&self, old: Self, node: &Node) {
        self.node.update(old.node, node);
        if self.event != old.event {
            unimplemented!()
        }
        old.closure.swap(&self.closure);
        // TODO
    }
}

impl<N: SingleElement, F> SingleElement for Listener<N, F> where Listener<N, F>: SingleNode {}

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
pub struct KeyedNode<K, T> {
    key: K,
    value: T,
    node: Cell<Option<Node>>,
}

impl<K, T> KeyedNode<K, T> {
    pub fn of(key: K, value: T) -> Self {
        KeyedNode {
            key,
            value,
            node: Cell::new(None),
        }
    }
}

impl<K: Eq + Hash, T: SingleNode> Vnode for Vec<KeyedNode<K, T>> {
    fn patch(ctx: &mut Context, p: Option<Self>, n: Option<&Self>) {
        let empty = Vec::new();
        let (p, n) = (p.unwrap_or(Vec::new()), n.unwrap_or(&empty));
        struct Cb<K, T> {
            key_type: PhantomData<K>,
            value_type: PhantomData<T>,
            parent: Node,
            /// The rightmost node last touched.
            right: Option<Node>,
            /// The index of the leftmost unchanged new node last touched.
            left_j: usize,
        };
        impl<K: Eq + Hash, T: SingleNode> DiffCallback<KeyedNode<K, T>, (usize, &KeyedNode<K, T>)>
            for Cb<K, T>
        {
            fn inserted(&mut self, (_j, new): (usize, &KeyedNode<K, T>)) {
                let node = new.value.create_node();
                self.right = Some(
                    self.parent
                        .insert_before(&node, self.right.as_ref())
                        .expect("Failed to insert node"),
                );
                new.node.set(Some(node));
            }
            fn removed(&mut self, old: KeyedNode<K, T>) {
                if let Some(node) = old.node.replace(None) {
                    self.parent
                        .remove_child(&node)
                        .expect("Failed to remove node");
                } else {
                    unreachable!()
                }
            }
            fn unchanged(&mut self, old: KeyedNode<K, T>, (j, new): (usize, &KeyedNode<K, T>)) {
                let node = old.node.replace(None).expect("Should have node");
                new.value.update(old.value, &node);
                if self.left_j == j {
                    self.left_j += 1;
                } else {
                    self.right = Some(node.clone());
                }
                new.node.set(Some(node));
            }
            fn moved(&mut self, old: KeyedNode<K, T>, (_j, new): (usize, &KeyedNode<K, T>)) {
                let node = old.node.replace(None).expect("Should have node");
                self.right = Some(
                    self.parent
                        .insert_before(&node, self.right.as_ref())
                        .expect("Failed to move node"),
                );
                new.value.update(old.value, &node);
                new.node.set(Some(node));
            }
        }
        let (parent, right) = match &ctx.cursor {
            Cursor::Parent(parent) => (parent.clone(), None),
            Cursor::Child(sibling) => (sibling.parent_node().unwrap(), Some(sibling.clone())),
        };
        let mut cb = Cb {
            key_type: PhantomData,
            value_type: PhantomData,
            parent,
            right,
            left_j: 0,
        };
        diff_by_key(
            p.into_iter(),
            |x| &x.key,
            n.iter().enumerate(),
            |x| &x.1.key,
            &mut cb,
        );
        // Hackish way to set cursor to first new node
        if let Some(first) = n.first() {
            let node = first.node.replace(None).unwrap();
            ctx.cursor = Cursor::Child(node.clone());
            first.node.set(Some(node));
        }
    }
}

pub mod hook;

#[cfg(test)]
mod tests {
    use super::{html, tags::div};

    #[test]
    fn macro_self_closing_tag() {
        assert_eq!(html! {<div />}, div)
    }

    #[test]
    #[should_panic]
    fn macro_mismatched_tags() {
        html! {<div></button>};
    }

    #[test]
    fn macro_expression_block() {
        assert_eq!(
            html! {{
                let node = div;
                node
            }},
            div
        );
    }
}
