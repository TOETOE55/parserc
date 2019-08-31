mod parser;
mod another;
mod other;
use parser::*;
use std::rc::Rc;
use std::marker::PhantomData;

fn num() -> impl Parser<Target=u64> {
    satisfy(|ch| ch.is_numeric())
        .some()
        .map(Vec::into_iter)
        .map(|iter| iter.collect::<String>())
        .map(|num| num.parse::<u64>())
        .map(Result::unwrap)
}

// a+(b|c)
fn aaab_c() -> impl Parser<Target=()> {
    char('a').some()
        .and(char('b').or(char('c')))
        .and(pure(||{}))
        .or(failure())

}

fn dynamic() -> impl Parser<Target=char> {
    num().and_then(|n| if n%2 == 0 {
        Box::new(satisfy(|ch| ch.is_uppercase()))
            as Box<dyn Parser<Target=char>>
    } else {
        Box::new(satisfy(|ch| ch.is_lowercase()))
    })
}

fn recursion() -> impl Parser<Target=()> {
    fix(Rc::new(|this|
        Box::new(char('.').and(this)
            .or(char('^')))))
        .and(pure(|| { }))
}

fn main() {
    let mut src = ParseState::new("2H");
    let res = dynamic().parse(&mut src);
    assert_eq!(res, Some('H'));
    println!("{}", src.src.as_str());

    let mut src = ParseState::new("123v");
    let res = num().parse(&mut src);
    assert_eq!(res, Some(123));
    println!("{}", src.src.as_str());

    let mut src = ParseState::new("123v");
    let res = strg("123").parse(&mut src);
    assert_eq!(res, Some("123"));
    println!("{}", src.src.as_str());


    let mut src = ParseState::new("...^");
    let parser = recursion();
    let res = parser.parse(&mut src);
    assert_eq!(res, Some(()));
    println!("{}", src.src.as_str());
}

