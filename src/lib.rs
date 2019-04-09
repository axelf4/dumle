use web_sys::Node;

trait Diff {
    // TODO want check for some/none to be done at compile time
    fn diff(ctx: &mut Context, p: Option<&Self>, n: Option<&Self>)
        where Self: Sized;
}

pub trait Vnode {
    const NAME: &'static str;
}

// Ensure that updating div is completely zero-cost
#[allow(non_camel_case_types)]
pub struct div;

impl Vnode for div {
    const NAME: &'static str = "div";
}

pub struct BooleanAttribute {
    /// The name of the DOM attribute.
    /// Const-generics could help here.
    name: &'static str,
    /// The current value of this attribute.
    value: bool,
}

/// Context during rendering.
pub struct Context {
    /// The current DOM node.
    pub cursor: Node,
}

pub fn element(ctx: Context) {
    let document = web_sys::window().unwrap().document().unwrap();
    let child = document.create_element("button").unwrap().into();
    ctx.cursor.append_child(&child);
}
