# 用Rust愉快地编写Parser Combinator

本人有个小小的习惯，就是在学习一门语言的开始，为了熟悉这门语言的基础设施，我都会写一个最最简单的parserc（当然那些9012年都没有支持泛型的语言，就不写了）。

这次就试着用Rust来写一个。



## 从Iterator中来

用过Rust的Iterator的人，一定会觉得这用起来十分的愉悦，这说明Iterator设计得很不错。而parserc的使用方式其实和使用Rust的Iterator的方式十分相似的——先将小的组合子构造成大的组合子，然后再使用，parserc是`.parse()`，Iterator是`.next()`。

所以借鉴一下Iterator的思路，Rust版parserc也试着由这几部分构成：

1. parser的trait
2. 一些adapter
3. 自定义一些combinator
4. 组合出来的一些combinator

用起来大概是这样：

```rust
// a+(b|c)
fn aaab_c() -> impl Parser<Target=()> {
    char('a').some()
        .and(char('b').or(char('c')))
        .map(|_| ())
}

fn main() {
    let mut src = ParseState::new("aaab");
    let res = aaab_c().parse(&mut src);
    assert_eq!(res, Some(()));
}
```



## Parser trait

先定义parser的一般行为`parse`，“照搬”Iterator的结构。

```rust
#[derive(Clone, Debug)]
pub struct ParseState<'a> {
    src: Chars<'a>,
    pub col: usize,
    pub row: usize,
}

pub trait Parser {
    type Target;
    fn parse<'a>(&self, state: &mut ParseState<'a>) -> Option<Self::Target>;
}
```

在Haskell中，最简单的parse function的类型是`String -> Maybe (a, String)`：吞进去一个String，得到解析结果a，还有未匹配的字符串，或者没有结果，也就是匹配失败。这里也采用了类似的结构，有所不同的是，因为Rust是有mut的，可以直接改变状态，于是可以去掉用返回值表示的状态，改为可变引用；然后，parse的状态加上了行和列，为了方便，用字符迭代器表示要解析的字符串。



## 第一个combinator

有了parser trait，我们可以来定义一个简单的combinator：`char`用来匹配一个字符。

```rust
/// char
pub struct Char {
    ch: char, // 要判断的字符
}

impl Parser for Char {
    type Target = char; // 解析结果的类型是字符型
    fn parse<'a>(&self, state: &mut ParseState<'a>) -> Option<Self::Target> {
        let old = state.clone();
        state.src.next()
            .filter(|&ch| self.ch == ch) // 判断字符是否相等
            .map(|ch| {
                state.update_pos(ch); // 更新位置
                ch
            }).or_else(|| {
                *state = old; // 错误恢复
                None
            })
    }
}

pub fn char(ch: char) -> Char {
    Char { ch }
}
```

现在就可以使用char这个combinator了

```rust
fn main() {
    let mut src = ParseState::new("aaab");
    let res = char('a').parse(&mut src);
    assert_eq!(res, Some('a'));
}
```



复杂一点点：`satisfy`组合子，用来判断字符是否满足某个表达式：

```rust
pub struct Satisfy<F> {
    satisfy: F, // 表达式
}

impl<F> Parser for Satisfy<F>
    where F: Fn(&char) -> bool,
{
    type Target = char;
    fn parse<'a>(&self, state: &mut ParseState<'a>) -> Option<Self::Target> {
        let old = state.clone();
        state.src.next()
            .filter(&self.satisfy) // 判断下个字符是否满足表达式
            .map(|ch| {
                state.update_pos(ch); // 更新位置
                ch
            }).or_else(|| {
                *state = old; // 错误恢复
                None
            })
    }
}

pub fn satisfy<F>(f: F) -> Satisfy<F>
    where F: Fn(&char) -> bool,
{
    Satisfy { satisfy: f }
}
```

使用起来：

```rust
fn main() {
    let mut src = ParseState::new("aaab");
    let res = satisfy(|ch| ch.is_uppercase()) // 匹配大写字符
        .parse(&mut src);
    assert_eq!(res, None);
}
```



接下来可以用类似的方法定义很多很多的combinator了，比如匹配一段字符的，匹配数字的，甚至匹配正则表达式的。（然而我没写）

还有，错误恢复的逻辑是需要的，这单独抽出来：

```rust
fn recover<T, S>(state: &mut S, old: S, opt: Option<T>) -> Option<T> {
    opt.or_else(|| {
        *state = old;
        None
    })
}
```



## 一些adapter

有了我们自己定义的一个个combinator之后，我们当然可以“裸着”调用，先parse a再parse b，但这样非常不composable。需要定义一些像Iterator中Map，Chain，Zip这些adapter，用来组合parser combinator。

那对于一个parser来说，顺序匹配`and`，和选择`or`是比较常用的combinator，我们先来看看怎么定义他们。



`and`，顺序匹配两个combinator，取第二个combinator的结果：

```rust
/// and
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
```

   然后在Parser trait里加上：

```rust
fn and<P>(self, other: P) -> And<Self, P> where
    Self: Sized
{
        And { a: self, b: other }
}
```

   十分简单。



`or`，选择匹配两个组合子，若第一个失败则重新匹配第二个：

```rust
pub struct Or<A, B> {
    a: A,
    b: B,
}

impl<A, B> Parser for Or<A, B> where
    A: Parser,
    B: Parser<Target=A::Target> , // 为了保证两个组合子得到的结果具有一样的类型
{
    type Target = A::Target;
    fn parse<'a>(&self, state: &mut ParseState<'a>) -> Option<Self::Target> {
        self.a.parse(state).or_else(|| self.b.parse(state))
    }
}
```

   然后同时在Parser trait里加上：

```rust
fn or<P>(self, other: P) -> Or<Self, P> where
    Self: Sized, P: Parser<Target=Self::Target>
{
    Or { a: self, b: other }
}
```



`map`组合子，不进行匹配，给里面的值做变换：

```rust
/// map
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
```



`and_then`组合子（就是`>>=`，实现上下文相关的匹配）

```rust
pub struct AndThen<P, F> {
    parser: P,
    f: F,
}

impl<A, B, F> Parser for AndThen<A, F> where
    A: Parser,
    B: Parser,
    F: Fn(A::Target) -> B, // 注意：这里返回的类型只有一个
{
    type Target = B::Target;
    fn parse<'a>(&self, state: &mut ParseState<'a>) -> Option<Self::Target> {
        let old = state.clone();
        let res = self.parser.parse(state)
            .and_then(|a| (self.f)(a).parse(state)); // 根据第一个匹配的结果选择parser继续匹配。
        recover(state, old, res)      
    }
}
```



还可以实现`many`和`some`组合子（匹配多次）等。



其实，这都是将平时使用parser function的一些普遍的pattern抽象成出来作为一些adapter而已。值的注意的是，将他们组合起来的时候，其实并没有发生parse的计算，做的仅仅是将一个个结构体包在一起而已。



## 堆起来

总之，我们可以将parser组合起来了，名副其实的combinator：

```rust
// a+(b|c)
fn aaab_c() -> impl Parser<Target=()> {
    char('a').some()
        .and(char('b').or(char('c')))
        .map(|_| ())
}
```

不过其实到目前为止还是“暗含危机”的：

```rust
fn demo() -> impl Parser<Target=char> {
    num().and_then(|n| if n%2 == 0 { // 匹配数字
        satisfy(|ch| ch.is_uppercase()) // 数字是偶数，则接下来匹配大写字母
    } else {
        satisfy(|ch| ch.is_lowercase()) // 数字是奇数，则接下来匹配小写字母
    })
}
```

这段代码逻辑是很简单，也是很常见的需求。但是这段代码是无法通过编译的。

为什么呢？给读者思考2秒钟……

1……

2……

ok！揭晓：**是因为两个satisfy返回的组合子类型不一样！**（别忘了在Rust里，每一个闭包的类型都是不一样的）

这的确是个棘手的问题呢！如果`and_then`里不能放不同的parserc，这个功能不就是残废的吗？



## 静中生动

到目前为止，咱们的parserc都是静态派分的，还并没有用到动态派分的功能。事实上上面遇到的问题，闭包就常常有这样的问题，这时候就要用到**trait object**了。

所谓trait object一般指的是一个指向类型为`dyn trait`的对象的胖指针。比如说实现了`Fn()`的闭包都可以被装到`Box<dyn Fn()>`的指针中。而因为`Box<dyn Fn()>`也实现了`Fn()`，所以也可以当做函数被调用。在这里trait object就做了「类型擦除」的工作。

同样的，我希望parserc也可以被包在Box或者其它指针中成为trait object，消除上面遇到的问题：

```rust
fn demo() -> impl Parser<Target=char> {
    num().and_then(|n| if n%2 == 0 {
        Box::new(satisfy(|ch| ch.is_uppercase())) 
            as Box<dyn Parser<Target=char>> // 别忘了要型转
    } else {
        Box::new(satisfy(|ch| ch.is_lowercase()))
    })
}
```

当然，现在是不行的，因为`Box<dyn Parser>`还没实现Parser，现在加上：

```rust
// 别忘了加?Sized，因为dyn trait是DST，是不确定大小的类型
impl<P: Parser + ?Sized> Parser for Box<P> {
    type Target = P::Target;
    fn parse<'a>(&self, state: &mut ParseState<'a>) -> Option<Self::Target> {
        (**self).parse(state) // deref一次&，一次Box，不然就会递归调用
    }
}
```

其它指针的也可以加上（



## 总结

到目前为止，就得到了一个很简单地Rust版parserc。写Rust，脑子里还是少带点monad，写起来还是十分愉悦的。想起知乎上也有篇文章是Rust版parsec，好像是很复杂很复杂的，各种Rc什么的，不知道这里要实现类似的功能要不要变得如此复杂呢……？

不过就算写了个parserc，我觉得我还是没有入门rust，毕竟前面还有啥Send/Sync，async/await，unsafe等着我。（诶，你说async/await还是没有稳定？）



## 后记

其实在我大半年前刚接触Rust的时候，我就干过这样的事。想0基础，硬撸parserc，结果被rustc打得鼻青脸肿，直接导致了我后来一直都没有再碰Rust，直至到上个学期末试着用Rust写操作系统的作业。。



为了再现当时的惨像，我又写了过去的代码：

当时的想法就是直接将Haskell的代码搬过去（java和scala的parserc我也是这样做的），直接包一个Parser的类型`String -> (a, String)`：

```rust
pub struct Parser<A, F> where
    F: for<'a> Fn(&mut Chars<'a>) -> Option<A>,
{
    p: F, // 不过当时不是这样的，是用Box将里面的parse function包起来
    _a: PhantomData<A>
    ,
}
```

嗯，第一个组合子就碰壁了：

```rust
fn pure<A>(x: A) -> Parser<A, impl for<'a> Fn(&mut Chars<'a>) -> Option<A>> {
    Parser {
        p: move |s| Some(x) ,
        _a: PhantomData ,
    }
}
```

报错是`cannot move out of captured outer variable in an  Fn closure`。嘛，现在明白原因了，因为Fn是“可以多次调用的闭包”，但是当调用parser的时候，x就被move出来了，就不能调用第二次了。为了方便就给A加上了Clone的约束。



接下来是`and_then` (`>>=`)，记得当时死活写不出来的：

```rust
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
```

……？写出来了？`or`(`<|>`)呢？

```rust
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
```

`satisfy`组合子

```rust
pub fn satisfy<F: Fn(&char) -> bool>(f: F)
    -> Parser<char, impl for<'a> Fn(&mut Chars<'a>) -> Option<char>> {
    Parser::new(move |chs: &mut Chars| chs.next().filter(&f))
}
```



……………………瞬间有一种之前的代码白给了的感觉。死活想不起来，为什么之前写不出来。不过，按照设计模式上说，还是前面的设计比较好一点，在usage也不需要写后面impl一串东西。



后后记：又尝试将parse function用Box包起来之后，生命期就变得复杂了，我当时没搞懂的估计也就是这个……现在折腾一下也ok：

```rust
// parse function生命期为'f，A活的至少比parse function久
pub struct Parser<'f, A: 'f> {
    p: Box<for<'str> Fn(&mut Chars<'str>) -> Option<A> + 'f>,
}
```

