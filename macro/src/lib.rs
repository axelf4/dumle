extern crate proc_macro;
use self::proc_macro::TokenStream;

use syn::{parse_macro_input, parse_quote, Expr, Ident, ItemFn, Local, Pat, Stmt, Token};

#[proc_macro_attribute]
pub fn render(attr: TokenStream, item: TokenStream) -> TokenStream {
    println!("proc macro render");

    let input = proc_macro2::TokenStream::from(item);

    let output: proc_macro2::TokenStream = {
        /* transform input */
        input
    };

    proc_macro::TokenStream::from(output)
}
