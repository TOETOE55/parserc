use std::str::Chars;
use std::rc::Rc;

// parse function生命期为'f，A的生命期至少比'f要长
#[derive(Clone)]
pub struct Parser<'f, A: 'f> {
    p: Rc<for<'str> Fn(&mut Chars<'str>) -> Option<A> + 'f>,
}

fn recover<T, S>(state: &mut S, old: S, opt: Option<T>) -> Option<T> {
    opt.or_else(|| {
        *state = old;
        None
    })
}


impl<'fa, A> Parser<'fa, A> {
    pub fn new<F>(p: F) -> Self where
        F: for<'str> Fn(&mut Chars<'str>) -> Option<A> + 'fa,
    {
        Parser { p: Rc::new(p) }
    }

    pub fn parse(&self, src: &mut Chars) -> Option<A> {
        (self.p)(src)
    }

    pub fn and_then<B: 'fa, Map: 'fa>(self, f: Map) -> Parser<'fa, B>
        where
            Map: Fn(A) -> Parser<'fa, B>,

    {
        Parser::new(move |chs| {
            let old = chs.clone();
            let res = self.parse(chs)
                .and_then(|a| f(a).parse(chs));
            recover(chs, old, res)
        })

    }

    pub fn or(self, other: Parser<'fa, A>) -> Parser<'fa, A> {
        Parser::new(move |chs: &mut Chars| self.parse(chs).or_else(|| other.parse(chs)))
    }
}

pub fn satisfy<'t, F: Fn(&char) -> bool + 't>(f: F) -> Parser<'t, char> {
    Parser::new(move |chs: &mut Chars| {
        let old = chs.clone();
        let res = chs.next().filter(&f);
        recover(chs, old, res)
    })
}