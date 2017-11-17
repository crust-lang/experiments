extern crate proc_macro;
use proc_macro::TokenStream;

extern crate syn;

#[macro_use]
extern crate quote;

#[proc_macro_derive(MyMacro)]
pub fn my_macro(input: TokenStream) -> TokenStream {
    let source = input.to_string();

    // Parse the string representation into a syntax tree
    let ast = syn::parse_derive_input(&source).unwrap();

    // Build the output, possibly using quasi-quotation
    let expanded = quote! {
        // ...
    };

    // Parse back to a token stream and return it
    expanded.parse().unwrap()
}

#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {
    }
}
