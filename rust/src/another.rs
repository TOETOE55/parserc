use std::str::Chars;
use std::marker::PhantomData;

pub struct Parser<A, F> where
    F: for<'a> Fn(&mut Chars<'a>) -> Option<A>,
{
    p: F,
    _a: PhantomData<A>,
}

/*
// Fn要用多次，但是里面的p被调用一次之后，A就会move出来，就不行了……
fn pure<A>(x: A) -> Parser<A, impl for<'a> Fn(Chars<'a>) -> Option<A>> {
    Parser {
        p: move |s| Some(x),
        _a: PhantomData
    }
}
*/
/*
fn pure<'t, A>(x: &'t A) -> Parser<&'t A, impl for<'a> Fn(Chars<'a>) -> Option<&'t A>> {
    Parser {
        p: move |_s| Some(x),
        _a: PhantomData
    }
}
*/

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

pub fn pure<A: Clone>(x: A) -> Parser<A, impl for<'a> Fn(&mut Chars<'a>) -> Option<A>> {
    Parser::new(move |_s| Some(x.clone()))
}

pub fn satisfy<F: Fn(&char) -> bool>(f: F)
    -> Parser<char, impl for<'a> Fn(&mut Chars<'a>) -> Option<char>> {
    Parser::new(move |chs: &mut Chars| {
        let old = chs.clone();
        let res = chs.next().filter(&f);
        recover(chs, old, res)
    })
}






//////////////////////////////////////////////////////////////////////////////
// parse function生命期为'f，A的生命期至少比'f要长
pub struct ParserB<'f, A: 'f> {
    p: Box<for<'str> Fn(&mut Chars<'str>) -> Option<A> + 'f>,
}

impl<'fa, A> ParserB<'fa, A> {
    pub fn new<F>(p: F) -> Self where
        F: for<'str> Fn(&mut Chars<'str>) -> Option<A> + 'fa,
    {
        ParserB { p: Box::new(p) }
    }

    pub fn parse(&self, src: &mut Chars) -> Option<A> {
        (self.p)(src)
    }

    pub fn and_then<B: 'fa, Map: 'fa>(self, f: Map) -> ParserB<'fa, B>
        where
            Map: Fn(A) -> ParserB<'fa, B>,

    {
        ParserB::new(move |chs| {
            let old = chs.clone();
            let res = self.parse(chs)
                .and_then(|a| f(a).parse(chs));
            recover(chs, old, res)
        })

    }

    pub fn or(self, other: ParserB<'fa, A>) -> ParserB<'fa, A> {
        ParserB::new(move |chs: &mut Chars| {
            let old = chs.clone();
            let first = self.parse(chs);
            recover(chs, old, first).or_else(|| other.parse(chs))
        })
    }
}

pub fn satisfy_b<'t, F: Fn(&char) -> bool + 't>(f: F) -> ParserB<'t, char> {
    ParserB::new(move |chs: &mut Chars| {
        let old = chs.clone();
        let res = chs.next().filter(&f);
        recover(chs, old, res)
    })
}