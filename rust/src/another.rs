use std::str::Chars;
use std::marker::PhantomData;

pub struct Parser<A, F> where
    F: for<'a> Fn(&mut Chars<'a>) -> Option<A>,
{
    p: F,
    _a: PhantomData<A>,
}


fn recover<T, S>(state: &mut S, old: S, opt: Option<T>) -> Option<T> {
    opt.or_else(|| {
        *state = old;
        None
    })
}

impl<A, F> Parser<A, F> where
    F: for<'a> Fn(&mut Chars<'a>) -> Option<A>,
{
    pub fn parse(&self, src: &mut Chars) -> Option<A> {
        (self.p)(src)
    }

    pub fn new(p: F) -> Self {
        Parser { p, _a: PhantomData, }
    }

    pub fn and_then<B, Map, G>(self, f: Map)
                               -> Parser<B, impl for<'a> Fn(&mut Chars<'a>) -> Option<B>>
        where
        Map: Fn(A) -> Parser<B, G>,
        G: for<'a> Fn(&mut Chars<'a>) -> Option<B>
    {
        Parser::new(move |chs| {
            let old = chs.clone();
            let res = self.parse(chs)
                .and_then(|a| f(a).parse(chs));
            recover(chs, old, res)
        })
    }

    pub fn or<G>(self, other: Parser<A, G>)
                 -> Parser<A, impl for<'a> Fn(&mut Chars<'a>) -> Option<A>>
        where
        G: for<'a> Fn(&mut Chars<'a>) -> Option<A>
    {
        Parser::new(move |chs: &mut Chars| {
            let old = chs.clone();
            let first = self.parse(chs);
            recover(chs, old, first).or_else(|| other.parse(chs))
        })
    }
}

pub fn pure<A, F: Fn() -> A>(f: F) -> Parser<A, impl for<'a> Fn(&mut Chars<'a>) -> Option<A>> {
    Parser::new(move |_s| Some(f()))
}

pub fn satisfy<F: Fn(&char) -> bool>(f: F)
    -> Parser<char, impl for<'a> Fn(&mut Chars<'a>) -> Option<char>> {
    Parser::new(move |chs: &mut Chars| {
        let old = chs.clone();
        let res = chs.next().filter(&f);
        recover(chs, old, res)
    })
}
