use std::str::Chars;

#[derive(Clone, Debug)]
pub struct ParseState<'a> {
    src: Chars<'a>,
    pub col: usize,
    pub row: usize,
}

impl<'a> ParseState<'a> {
    pub fn new(src: &'a str) -> Self {
       ParseState {
           src: src.chars(),
           col: 0,
           row: 0,
       }
    }

    fn update_pos(&mut self, ch: char) {
        match ch {
            '\n' => self.row +=1 ,
            '\t' => self.col = self.col+8 - (self.col-1)%8 ,
            _    => self.col += 1 ,
        }
    }
}

impl<'a> Iterator for ParseState<'a> {
    type Item = char;
    fn next(&mut self) -> Option<Self::Item> {
        self.src.next().map(|ch| {
            match ch {
                '\n' => self.row +=1 ,
                '\t' => self.col = self.col+8 - (self.col-1)%8 ,
                _    => self.col += 1 ,
            }
            ch
        })
    }
}

pub trait Parser {
    type Target;
    fn parse<'a>(&self, state: &mut ParseState<'a>) -> Option<Self::Target>;

    fn or<P>(self, other: P) -> Or<Self, P> where
        Self: Sized, P: Parser<Target=Self::Target>
    {
        Or { a: self, b: other }
    }

    fn and<P>(self, other: P) -> And<Self, P> where
        Self: Sized
    {
        And { a: self, b: other }
    }

    fn map<B, F>(self, f: F) -> Map<Self, F> where
        Self: Sized, F: Fn(Self::Target) -> B,
    {
        Map { parser: self, f }
    }

    fn and_then<P, F>(self, f: F) -> AndThen<Self, F> where
        Self: Sized,
        P: Parser,
        F: Fn(Self::Target) -> P,
    {
        AndThen { parser: self, f }
    }

    fn many(self) -> Many<Self> where Self: Sized {
        Many { parser: self }
    }

    fn some(self) -> Some<Self> where Self: Sized {
        Some { parser: self }
    }
}

fn recover<T, S>(state: &mut S, old: S, opt: Option<T>) -> Option<T> {
    opt.or_else(|| {
        *state = old;
        None
    })
}

impl<P: Parser + ?Sized> Parser for &P {
    type Target = P::Target;
    fn parse<'a>(&self, state: &mut ParseState<'a>) -> Option<Self::Target> {
        (**self).parse(state)
    }
}

impl<P: Parser + ?Sized> Parser for &mut P {
    type Target = P::Target;
    fn parse<'a>(&self, state: &mut ParseState<'a>) -> Option<Self::Target> {
        (**self).parse(state)
    }
}

impl<P: Parser + ?Sized> Parser for Box<P> {
    type Target = P::Target;
    fn parse<'a>(&self, state: &mut ParseState<'a>) -> Option<Self::Target> {
        (**self).parse(state)
    }
}

impl<P: Parser + ?Sized> Parser for std::rc::Rc<P> {
    type Target = P::Target;
    fn parse<'a>(&self, state: &mut ParseState<'a>) -> Option<Self::Target> {
        (**self).parse(state)
    }
}

/// parser combinator

/// pure
#[derive(Clone)]
pub struct Pure<Target> {
    x: Target
}

impl<Target: Clone> Parser for Pure<Target> {
    type Target = Target;
    fn parse<'a>(&self, _state: &mut ParseState<'a>) -> Option<Self::Target> {
        Some(self.clone().x)
    }
}

pub fn pure<T>(x: T) -> Pure<T> {
    Pure { x }
}

/// satisfy
#[derive(Clone)]
pub struct Satisfy<F> {
    satisfy: F,
}

impl<F> Parser for Satisfy<F>
    where F: Fn(&char) -> bool,
{
    type Target = char;
    fn parse<'a>(&self, state: &mut ParseState<'a>) -> Option<Self::Target> {
        let old = state.clone();
        let res = state.src.next()
            .filter(&self.satisfy)
            .map(|ch| {
                state.update_pos(ch);
                ch
            });

        recover(state, old, res)
    }
}

pub fn satisfy<F>(f: F) -> Satisfy<F>
    where F: Fn(&char) -> bool,
{
    Satisfy { satisfy: f }
}


/// char
#[derive(Clone)]
pub struct Char {
    ch: char,
}

impl Parser for Char {
    type Target = char;
    fn parse<'a>(&self, state: &mut ParseState<'a>) -> Option<Self::Target> {
        let old = state.clone();
        let res = state.src.next()
            .filter(|&ch| self.ch == ch)
            .map(|ch| {
                state.update_pos(ch);
                ch
            });

        recover(state, old, res)
    }
}

pub fn char(ch: char) -> Char {
    Char { ch }
}

/// strg
#[derive(Clone)]
pub struct Strg<'a> {
    s: &'a str,
}

impl<'s> Parser for Strg<'s> {
    type Target = &'s str;
    fn parse<'a>(&self, state: &mut ParseState<'a>) -> Option<Self::Target> {
        let old = state.clone();
        let mut chars = self.s.chars();
        loop {
            match chars.next() {
                Option::None    => return Some(self.s) ,
                Option::Some(a) => match state.next() {
                    Option::None    => return None ,
                    Option::Some(b) => if a != b {
                        *state = old;
                        return None
                    } ,
                }
            }
        }
    }
}

pub fn strg(s: &str) -> Strg {
    Strg { s }
}

/// adaptor
/// or
#[derive(Clone)]
pub struct Or<A, B> {
    a: A,
    b: B,
}

impl<A, B> Parser for Or<A, B> where
    A: Parser,
    B: Parser<Target=A::Target>,
{
    type Target = A::Target;
    fn parse<'a>(&self, state: &mut ParseState<'a>) -> Option<Self::Target> {
        let old = state.clone();
        let first = self.a.parse(state);

        recover(state, old, first).or_else(|| self.b.parse(state))
    }
}

/// and
#[derive(Clone)]
pub struct And<A, B> {
    a: A,
    b: B,
}

impl<A: Parser, B: Parser> Parser for And<A, B> {
    type Target = B::Target;
    fn parse<'a>(&self, state: &mut ParseState<'a>) -> Option<Self::Target> {
        let old = state.clone();
        let res = self.a.parse(state).and(self.b.parse(state));

        recover(state, old, res)
    }
}

/// map
#[derive(Clone)]
pub struct Map<P, F> {
    parser: P,
    f: F,
}

impl<B, P: Parser, F> Parser for Map<P, F>
    where F: Fn(P::Target) -> B,
{
    type Target = B;
    fn parse<'a>(&self, state: &mut ParseState<'a>) -> Option<Self::Target> {
        self.parser.parse(state).map(&self.f)
    }
}

/// and_then
#[derive(Clone)]
pub struct AndThen<P, F> {
    parser: P,
    f: F,
}

impl<A, B, F> Parser for AndThen<A, F> where
    A: Parser,
    B: Parser,
    F: Fn(A::Target) -> B,
{
    type Target = B::Target;
    fn parse<'a>(&self, state: &mut ParseState<'a>) -> Option<Self::Target> {
        let old = state.clone();
        let res = self.parser.parse(state)
            .and_then(|a| (self.f)(a).parse(state));

        recover(state, old, res)
    }
}

/// many
pub struct Many<P> {
    parser: P,
}

impl<P: Parser> Parser for Many<P> {
    type Target = Vec<P::Target>;
    fn parse<'a>(&self, state: &mut ParseState<'a>) -> Option<Self::Target> {
        let mut vec = vec![];
        loop {
            let old = state.clone();
            match self.parser.parse(state) {
                Some(a) => vec.push(a),
                None => { *state = old; break; }
            }
        }
        Some(vec)
    }
}


/// some
#[derive(Clone)]
pub struct Some<P> {
    parser: P,
}

impl<P: Parser> Parser for Some<P> {
    type Target = Vec<P::Target>;
    fn parse<'a>(&self, state: &mut ParseState<'a>) -> Option<Self::Target> {
        let mut vec = vec![];
        let old = state.clone();
        if let Some(a) = self.parser.parse(state) {
            vec.push(a);
        } else { *state = old; return None; }

        loop {
            let old = state.clone();
            match self.parser.parse(state) {
                Some(a) => vec.push(a),
                None => { *state = old; break; }
            }
        }

        Some(vec)
    }
}