mod parser;
mod another;

use parser::*;

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
        .map(|_| ())

}

fn dynamic() -> impl Parser<Target=char> {
    num().and_then(|n| if n%2 == 0 {
        Box::new(satisfy(|ch| ch.is_uppercase()))
            as Box<dyn Parser<Target=char>>
    } else {
        Box::new(satisfy(|ch| ch.is_lowercase()))
    })
}

fn main() {
    let mut src = ParseState::new("2H");
    let res = dynamic().parse(&mut src);
    assert_eq!(res, Some('H'));

    let mut src = ParseState::new("123v");
    let res = num().parse(&mut src);
    assert_eq!(res, Some(123));

    let mut src = ParseState::new("123v");
    let res = strg("123").parse(&mut src);
    assert_eq!(res, Some("123"));

    let mut src = "1234".chars();
    let res =
        another::satisfy_b(|ch| ch.is_numeric())
            .and_then(|ch| another::satisfy_b(|ch| ch.is_numeric()))
            .parse(&mut src);
    assert_eq!(res, Some('2'));

}

