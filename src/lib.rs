pub mod clojure;

pub struct Object {}

pub struct Cons {
    first: Object,
    rest: Box<Cons>
}

#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {
    }
}
