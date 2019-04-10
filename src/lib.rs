use lis::{diff_by_key, DiffCallback};
use std::hash::Hash;
use web_sys::{Document, Node};

pub enum Cursor {
    /// Points at the parent. Behave as if right sibling doesn't exist.
    Parent(Node),
    /// Points at the previous sibling.
    Child(Node),
}

/// Context during rendering.
pub struct Context {
    /// The current DOM node.
    // Cursor needs to store parent and child
    cursor: Cursor,
}

impl Context {
    pub fn from(node: Node) -> Context {
        Context {
            cursor: Cursor::Parent(node),
        }
    }
}

/*pub fn element(ctx: Context) {
    let document = web_sys::window().unwrap().document().unwrap();
    let child = document.create_element("button").unwrap().into();
    ctx.cursor.append_child(&child);
}*/

pub fn patch<T: Vnode>(ctx: &mut Context, p: Option<T>, n: Option<&T>) {
    T::patch(ctx, p, n)
}
// We can have a trait for constant nodes
// and skip those when diffing already creates nodes

pub trait Vnode {
    // TODO want check for some/none to be done at compile time
    fn patch(ctx: &mut Context, p: Option<Self>, n: Option<&Self>)
    where
        Self: Sized;
}

/// Marker trait for Vnodes that correspond to a single DOM node.
trait SingleNode {}

// TODO Add text nodes

// FIXME Rewrite with const generics
pub trait Element {
    /// The name of the DOM element.
    const NAME: &'static str;
}

impl<E: Element> SingleNode for E {}

fn document() -> Document {
    web_sys::window().expect("").document().expect("")
}

impl<E: Element> Vnode for E {
    fn patch(ctx: &mut Context, p: Option<Self>, n: Option<&Self>) {
        match (p, n) {
            (None, Some(_)) => {
                let element = document()
                    .create_element(Self::NAME)
                    .expect("Failed to create element");
                element.set_text_content(Some("e")); // Debug

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
                .expect("This element not found");
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
                                .expect("This element is not foudn"),
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

    // TODO Ensure that updating div is completely zero-cost

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

pub struct ChildCons<P, C>(P, C);

impl<P: SingleNode, C: Vnode> Vnode for ChildCons<P, C> {
    fn patch(ctx: &mut Context, p: Option<Self>, n: Option<&Self>) {
        // TODO
    }
}

impl<P: SingleNode, C> SingleNode for ChildCons<P, C> {}

pub struct BooleanAttribute {
    /// The name of the DOM attribute.
    /// Const-generics could help here.
    name: &'static str,
    /// The current value of this attribute.
    value: bool,
}

pub struct Keyed<K, T> {
    key: K,
    value: T,
}

// Allow constructing cons lists
impl<A: Vnode, B: Vnode> Vnode for (A, B) {
    fn patch(ctx: &mut Context, p: Option<Self>, n: Option<&Self>) {
        let (pa, pb) = p.map(|(a, b)| (Some(a), Some(b))).unwrap_or((None, None));
        let (na, nb) = n.map(|(a, b)| (Some(a), Some(b))).unwrap_or((None, None));
        B::patch(ctx, pb, nb); // XXX: Do B first!
        A::patch(ctx, pa, na);
    }
}

pub struct Dyn<K, T>(Vec<Keyed<K, T>>);

impl<K: Eq + Hash, T: Vnode> Vnode for Dyn<K, T> {
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
