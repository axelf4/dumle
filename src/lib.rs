//! Virtual DOM library.
//!
//! The [html] macro is used for rendering virtual trees.
//! The real DOM can then be patched to match a virtual tree with [Context::patch].
//!
//! ## Example
//!
//! The following example creates a simple counter that can be incremented:
//!
//! ```no_run
//! use dumle::{hook::UseState, html, Context};
//! use wasm_bindgen::{prelude::*, throw_str, UnwrapThrowExt};
//!
//! #[wasm_bindgen(start)]
//! pub fn run() {
//!     // Get the document's `<body>`
//!     let body = web_sys::window()
//! 		.and_then(|window| window.document())
//! 		.and_then(|document| document.body())
//! 		.unwrap_throw();
//!
//! 	// Render a simple virtual node with a counter
//!     let vnode = UseState::new(|state: &i32, set_state| {
//! 		html! {
//! 			<div>
//! 				{ state.to_string() }
//! 				<button click=move |_| set_state(&|state: &mut i32| *state += 1),>
//! 					{"Increment"}
//! 				</button>
//! 			</div>
//! 		}
//! 	});
//!
//! 	// Patch the real DOM to match the virtual DOM
//!     Context::from(body.clone().into()).patch(None, Some(&vnode));
//!
//!     throw_str("SimulateInfiniteLoop") // Exit with live runtime
//! }
//! ```

#![deny(missing_docs, missing_debug_implementations)]

use lis::{diff_by_key, DiffCallback};
use std::cell::Cell;
use std::marker::PhantomData;
use std::{fmt, hash::Hash};
use wasm_bindgen::{prelude::*, JsCast, UnwrapThrowExt};
use web_sys::{Document, Event, EventTarget, Node};

/// Pointer to some DOM node.
#[derive(Clone, Debug)]
struct Cursor {
    /// The parent DOM node.
    parent: Node,
    /// The DOM node after the pointed at node, or `None` if it's the last child.
    child: Option<Node>,
}

/// Rendering context.
#[derive(Debug)]
pub struct Context {
    /// The current DOM node.
    cursor: Cursor,
}

impl Context {
    /// Returns a context for rendering into the specified DOM node.
    pub fn from(node: Node) -> Context {
        Context {
            cursor: Cursor {
                parent: node,
                child: None,
            },
        }
    }

    /// Patches the DOM to match the new virtual tree given the previous virtual tree.
    ///
    /// Consecutive patches must point at the same DOM node.
    pub fn patch<T: Vnode>(mut self, p: Option<T>, n: Option<&T>) {
        T::patch(&mut self, p, n)
    }
}

/// Returns the top-level document.
fn document() -> Document {
    web_sys::window().unwrap_throw().document().unwrap_throw()
}

/// Virtual DOM node.
pub trait Vnode {
    /// Patches the DOM to match the new virtual node given the previous.
    fn patch(ctx: &mut Context, p: Option<Self>, n: Option<&Self>)
    where
        Self: Sized;
}

/// Trait for virtual nodes that correspond to a single DOM node.
pub trait SingleNode {
    /// Returns a DOM representation of this virtual node.
    fn create_node(&self) -> Node;

    /// Updates this virtual node given the DOM node where it is mounted.
    ///
    /// The DOM node must change positions however it may be replaced.
    fn update(&self, _old: Self, _node: &mut Node)
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
                ctx.cursor.child = Some(
                    ctx.cursor
                        .parent
                        .insert_before(&node, ctx.cursor.child.as_ref())
                        .expect_throw("Failed to insert"),
                );
            }
            (Some(p), Some(n)) => {
                let mut node = ctx
                    .cursor
                    .child
                    .as_ref()
                    .map_or_else(|| ctx.cursor.parent.last_child(), Node::previous_sibling)
                    .expect_throw("This node is not found");
                n.update(p, &mut node);
                ctx.cursor.child = Some(node); // Update cursor
            }
            (Some(_), None) => {
                let node = ctx
                    .cursor
                    .child
                    .as_ref()
                    .map_or_else(|| ctx.cursor.parent.last_child(), Node::previous_sibling)
                    .expect_throw("This node is not found");
                ctx.cursor.parent.remove_child(&node).unwrap_throw();
            }
            _ => unreachable!(),
        }
    }
}

// TODO Once specialization lands: skip updating children if all are constant
/// Marker trait for virtual nodes that never need updating once mounted.
#[allow(unused)]
trait ConstantNode: Vnode {}

/// Marker trait for virtual nodes that correspond to a single DOM element.
pub trait SingleElement: SingleNode {}

/// Virtual DOM element with a given name.
pub trait Element {
    /// The name of the DOM element.
    const NAME: &'static str;
}

impl<E: Element> SingleNode for E {
    #[inline(always)]
    fn create_node(&self) -> Node {
        document()
            .create_element(Self::NAME)
            .expect_throw("Failed to create element")
            .into()
    }
}

impl<E: Element> SingleElement for E {}

/// Collection of HTML tags.
#[allow(non_camel_case_types, missing_docs)]
pub mod tags {
    use super::Element;

    macro_rules! macro_doc {
        ($x:expr, $($tt:tt)+) => {#[doc = $x] $($tt)+}
    }

    macro_rules! impl_tags {
        ($($name:ident),+) => {
            $(
                macro_doc!{
                    concat!("The HTML ", stringify!($name),
                    " tag. [MDN Documentation](https://developer.mozilla.org/en-US/docs/Web/HTML/Element/",
                    stringify!($name), ")"),
                    #[derive(PartialEq, Eq, Debug)]
                    pub struct $name;
                    impl Element for $name {
                        const NAME: &'static str = stringify!($name);
                    }
                }
            )+
        }
    }

    impl_tags! {
    a, abbr, acronym, address, applet, area, article, aside, audio, b, base, basefont, bdi,
    bdo, big, blockquote, body, br, button, canvas, caption, center, cite, code, col, colgroup,
    datalist, dd, del, details, dfn, dialog, dir, div, dl, dt, em, embed, fieldset,
    figcaption, figure, font, footer, form, frame, framset, h1, h2, h3, h4, h5, h6, head,
    header, hr, i, iframe, img, input, ins, kbd, keygen, label, legend, li, link, main, map,
    mark, menu, menuitem, meta, meter, nav, noframes, noscript, object, ol, optgroup, option,
    output, p, param, pre, progress, q, rp, rt, ruby, s, samp, script, section, select, small,
    source, span, strike, strong, style, sub, summary, sup, table, tbody, td, textarea, tfoot,
    th, thead, time, title, tr, track, tt, u, ul, var, video, wbr
    }
}

/// Virtual DOM text node with some data.
#[derive(PartialEq, Eq, Debug)]
pub struct Text<T>(pub T);

impl<T: AsRef<str> + PartialEq> SingleNode for Text<T> {
    #[inline(always)]
    fn create_node(&self) -> Node {
        document().create_text_node(self.0.as_ref()).into()
    }

    #[inline(always)]
    fn update(&self, old: Self, node: &mut Node) {
        if old.0 != self.0 {
            node.set_node_value(Some(self.0.as_ref()));
        }
    }
}

/// Allows construction of cons lists.
impl<A: Vnode, B: Vnode> Vnode for (A, B) {
    #[inline(always)]
    fn patch(ctx: &mut Context, p: Option<Self>, n: Option<&Self>) {
        let (pa, pb) = p.map(|(a, b)| (Some(a), Some(b))).unwrap_or((None, None));
        let (na, nb) = n.map(|(a, b)| (Some(a), Some(b))).unwrap_or((None, None));
        B::patch(ctx, pb, nb); // XXX: Do B first!
        A::patch(ctx, pa, na);
    }
}

/// Empty virtual node. Useful for `if` constructs using [Either].
impl Vnode for () {
    #[inline(always)]
    fn patch(_ctx: &mut Context, _old: Option<Self>, _new: Option<&Self>) {}
}

/// Virtual node wrapped with some child node.
#[derive(PartialEq, Eq, Debug)]
pub struct Child<P, C>(pub P, pub C);

impl<P: SingleNode, C: Vnode> SingleNode for Child<P, C> {
    #[inline(always)]
    fn create_node(&self) -> Node {
        let mut ctx = Context {
            cursor: Cursor {
                parent: self.0.create_node(),
                child: None,
            },
        };
        C::patch(&mut ctx, None, Some(&self.1));
        ctx.cursor.parent
    }
    #[inline(always)]
    fn update(&self, old: Self, node: &mut Node) {
        self.0.update(old.0, node);
        let mut ctx = Context {
            cursor: Cursor {
                parent: node.clone(),
                child: None,
            },
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

/// Convenience macro for building trees with an HTML-esque language.
///
/// ## Example
///
/// ```
/// use dumle::{html, tags, Child, Text};
/// assert_eq!(html! {
///     <div>
///         {"Text"}
///         <button />
///     </div>
/// }, Child(tags::div, (Text("Text"), tags::button)));
/// ```
#[macro_export]
macro_rules! html {
    ($($tt:tt)+) => {{$crate::html_impl!{() $($tt)+}}};
}

// TODO Parameter for type of value
// TODO Add toggle-able attributes using trait
/// Virtual node wrapped with an attribute.
#[derive(Debug)]
pub struct Attribute<N, V> {
    /// The name of the DOM attribute.
    name: &'static str,
    /// The current value of this attribute.
    value: V,
    /// The virtual node to set the attribute on.
    node: N,
}

impl<N: SingleElement, V: AsRef<str> + PartialEq> SingleNode for Attribute<N, V> {
    #[inline(always)]
    fn create_node(&self) -> Node {
        let node = self.node.create_node();
        node.unchecked_ref::<web_sys::Element>()
            .set_attribute(self.name, self.value.as_ref())
            .expect_throw("Failed to set attribute");
        node
    }

    #[inline(always)]
    fn update(&self, old: Self, node: &mut Node) {
        self.node.update(old.node, node);
        if self.value != old.value {
            node.unchecked_ref::<web_sys::Element>()
                .set_attribute(self.name, self.value.as_ref())
                .expect_throw("Failed to set attribute");
        }
    }
}

impl<N: SingleElement, V> SingleElement for Attribute<N, V> where Attribute<N, V>: SingleNode {}

/// Virtual node wrapped with an event listener.
pub struct Listener<N, F> {
    /// The case-sensitive event-type the listener listens for.
    event: &'static str,
    /// The listener callback function.
    callback: Cell<Option<F>>,
    #[doc(hidden)]
    closure: Cell<Option<Closure<FnMut(Event)>>>,
    /// The virtual node to add the listener to.
    node: N,
}

impl<N: SingleNode, F: FnMut(Event) + 'static> SingleNode for Listener<N, F> {
    fn create_node(&self) -> Node {
        let node = self.node.create_node();
        let cb = self.callback.take().unwrap_throw();
        let cb = Closure::wrap(Box::new(cb) as Box<dyn FnMut(Event)>);
        node.unchecked_ref::<EventTarget>()
            .add_event_listener_with_callback(self.event, cb.as_ref().unchecked_ref())
            .expect_throw("Failed to add event listener");
        self.closure.set(Some(cb));
        node
    }

    fn update(&self, old: Self, node: &mut Node) {
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
    /// Returns a new unattached [Listener].
    #[inline(always)]
    pub fn new(node: N, event: &'static str, callback: F) -> Self {
        Listener {
            event,
            callback: Cell::new(Some(callback)),
            closure: Cell::new(None),
            node,
        }
    }
}

impl<N: fmt::Debug, F> fmt::Debug for Listener<N, F> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "Listener {{ event: {}, node: {:?} }}",
            self.event, self.node
        )
    }
}

/// Trait for types that can be converted to [Attribute]/[Listener]:s.
pub trait ToAttribute<N: Vnode> {
    /// The output type of the conversion.
    type Output: Vnode;
    /// Converts this type given the name of the attribute/event-type.
    fn to_attribute(self, name: &'static str) -> Self::Output;
}

impl<N: SingleElement> ToAttribute<N> for (&'static str, N) {
    type Output = Attribute<N, &'static str>;
    #[inline(always)]
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
    #[inline(always)]
    fn to_attribute(self, event: &'static str) -> Self::Output {
        Listener::new(self.1, event, self.0)
    }
}

/// Trait for types that can be converted to a virtual node.
pub trait ToVnode {
    /// The output type of the conversion.
    type Output: Vnode;
    /// Converts this type into a virtual node.
    fn to_vnode(self) -> Self::Output;
}

impl<T: Vnode> ToVnode for T {
    type Output = Self;
    #[inline(always)]
    fn to_vnode(self) -> Self::Output {
        self
    }
}

impl ToVnode for &'static str {
    type Output = Text<Self>;
    #[inline(always)]
    fn to_vnode(self) -> Self::Output {
        Text(self)
    }
}

impl ToVnode for String {
    type Output = Text<Self>;
    #[inline(always)]
    fn to_vnode(self) -> Self::Output {
        Text(self)
    }
}

/// Virtual node that can be either one of two types.
#[derive(PartialEq, Eq, Debug)]
pub enum Either<A, B> {
    /// The first variant.
    A(A),
    /// The second variant.
    B(B),
}

impl<A: Vnode, B: Vnode> Vnode for Either<A, B> {
    fn patch(ctx: &mut Context, old: Option<Self>, new: Option<&Self>) {
        match (old, new) {
            (None, Some(new)) => match new {
                Either::A(new) => A::patch(ctx, None, Some(new)),
                Either::B(new) => B::patch(ctx, None, Some(new)),
            },
            (Some(old), Some(new)) => match old {
                Either::A(old) => match new {
                    Either::A(new) => A::patch(ctx, Some(old), Some(new)),
                    Either::B(new) => {
                        // TODO Add optimization using replaceNode for SingleNodes
                        A::patch(ctx, Some(old), None); // Remove old
                        B::patch(ctx, None, Some(new)); // Add new
                    }
                },
                Either::B(old) => match new {
                    Either::B(new) => B::patch(ctx, Some(old), Some(new)),
                    Either::A(new) => {
                        // TODO Add optimization using replaceNode for SingleNodes
                        B::patch(ctx, Some(old), None); // Remove old
                        A::patch(ctx, None, Some(new)); // Add new
                    }
                },
            },
            (Some(old), None) => match old {
                Either::A(old) => A::patch(ctx, Some(old), None),
                Either::B(old) => B::patch(ctx, Some(old), None),
            },
            (None, None) => unreachable!(),
        }
    }
}

/// Virtual node with an associated key.
pub struct KeyedNode<K, T> {
    /// The key.
    key: K,
    /// The virtual node.
    value: T,
    /// The DOM node if mounted, otherwise `None`.
    node: Cell<Option<Node>>,
}

impl<K, T> KeyedNode<K, T> {
    /// Returns a new [KeyedNode] with the specified key and virtual node.
    pub fn of(key: K, value: T) -> Self {
        KeyedNode {
            key,
            value,
            node: Cell::new(None),
        }
    }
}

impl<K: fmt::Debug, T: fmt::Debug> fmt::Debug for KeyedNode<K, T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "KeyedNode {{ key: {:?}, value: {:?} }}",
            self.key, self.value
        )
    }
}

impl<K: Eq + Hash, T: SingleNode> Vnode for Vec<KeyedNode<K, T>> {
    fn patch(ctx: &mut Context, p: Option<Self>, n: Option<&Self>) {
        let empty = Vec::new();
        let (p, n) = (p.unwrap_or_default(), n.unwrap_or(&empty));
        struct Cb<'a, K, T> {
            key_type: PhantomData<K>,
            value_type: PhantomData<T>,
            parent: &'a Node,
            /// The rightmost node last touched.
            right: Option<Node>,
            /// The index of the leftmost unchanged new node last touched.
            left_j: usize,
        };
        impl<K: Eq + Hash, T: SingleNode> DiffCallback<KeyedNode<K, T>, (usize, &KeyedNode<K, T>)>
            for Cb<'_, K, T>
        {
            fn inserted(&mut self, (_j, new): (usize, &KeyedNode<K, T>)) {
                let node = new.value.create_node();
                self.right = Some(
                    self.parent
                        .insert_before(&node, self.right.as_ref())
                        .expect_throw("Failed to insert node"),
                );
                new.node.set(Some(node));
            }
            fn removed(&mut self, old: KeyedNode<K, T>) {
                if let Some(node) = old.node.replace(None) {
                    self.parent
                        .remove_child(&node)
                        .expect_throw("Failed to remove node");
                } else {
                    unreachable!()
                }
            }
            fn unchanged(&mut self, old: KeyedNode<K, T>, (j, new): (usize, &KeyedNode<K, T>)) {
                let mut node = old.node.replace(None).expect_throw("Should have node");
                new.value.update(old.value, &mut node);
                if self.left_j == j {
                    self.left_j += 1;
                } else {
                    self.right = Some(node.clone());
                }
                new.node.set(Some(node));
            }
            fn moved(&mut self, old: KeyedNode<K, T>, (_j, new): (usize, &KeyedNode<K, T>)) {
                let mut node = old.node.replace(None).expect_throw("Should have node");
                self.right = Some(
                    self.parent
                        .insert_before(&node, self.right.as_ref())
                        .expect_throw("Failed to move node"),
                );
                new.value.update(old.value, &mut node);
                new.node.set(Some(node));
            }
        }
        let mut cb = Cb {
            key_type: PhantomData,
            value_type: PhantomData,
            parent: &ctx.cursor.parent,
            right: ctx.cursor.child.clone(),
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
            let node = first.node.replace(None).unwrap_throw();
            ctx.cursor.child = Some(node.clone());
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
