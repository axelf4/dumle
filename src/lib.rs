use web_sys::Node;

/// State of subtree traversal.
pub enum TraversalMode {
    /// Parent node is new. Complete creating is required.
    Creating,
    /// Updating parent node from last run. Only update differences.
    Updating,
    /// Parent node was removed. Process deletion hooks.
    Removing,
}

/// Context during rendering.
pub struct Context {
    pub cursor: Node,
    pub mode: TraversalMode,
    // TODO Add bump allocation arena
}

pub fn element(ctx: Context) {
    let document = web_sys::window().unwrap().document().unwrap();
    let child = document.create_element("button").unwrap().into();
    ctx.cursor.append_child(&child);
}
