# mtlParser

这是一个基于mtl和lens的简单的解析器组合子。

- 内置支持解析**上下文相关文法**的组合子，还提供强大的**回溯**功能，可以任意操作解析时的控制流。
- 提供一个parserc的typeclass（类似接口），可以自定义功能更多的组合子。



## 目录结构

>├─app
>│      Main.hs // 解析英语语法的主函数
>│
>└─src
>        Parser.hs // 解析器组合子
>        Syntax.hs // 描述英语语法的文件



## Usage


`float`是一个解析浮点数的解析器：

```haskell
evalParser float (initParseState "1.32") 
-- Right 1.32

runParser float (initParseState "333 ok") 
-- Right (0.333,ParseState {_inputState = "ok", _posState = Pos {_line = 0, _column = 6}})
-- "ok"是剩下的字符串，_posState是当前的位置

evalParser float (initParseState "abc")
-- Left (Err "err at Pos {_line = 0, _column = 0}")
-- 匹配失败
```

`many`组合子可以生成一个匹配0到无穷次的解析器，相当于正则表达式中的`*`：

```haskell
evalParser (many float) (initParseState "1 2 3 4")
-- Right [1.0,2.0,3.0,4.0]
-- many float相当于正则表达式float*
```

`<|>`组合子：

```haskell
evalParser (paren float <|> bracket float <|> brace float) (initParseState "(1.24)")
-- (float) or [float] or {float}
-- Right 1.24
```

当然还有很多内置的组合子。



还可以描述上下文相关的文法：

```haskell
ctxSensitive :: MonadParser ParseException (ParseState String) m => m Char
ctxSensitive = do
	a <- alpha
	case a of
		'^' -> upper 
		'_' -> lower
		_ -> throwError "err"
-- 当匹配的第一个字母为^时候，匹配下一个字符是大写的字母；_的时候匹配下一个为小写；否则中断当前匹配

evalParser ctxSensitive (initParseState "^A")
-- Right 'A'
evalParser ctxSensitive (initParseState "^a")
-- Left (Err ...)
evalParser ctxSensitive (initParseState "_a")
-- Right 'a'
```



`callCC`可以获取当前控制流，并且可以在任何时候回到这个时间点：

```haskell
try doSth handler = callCC $ \ok -> do -- ok是表示当前控制流的一个对象
	err <- callCC $ \notOk -> do -- notOk也是一个控制流
		x <- doSth notOk
		ok x
	handler err
-- 模拟try-catch

ctxSensitive' throw = do -- throw代表一个控制流
	a <- alpha
	case a of
		'^' -> upper 
		'_' -> lower
		_ -> throw "err" -- 返回到throw对应的一个控制流

test = try ctxSensitive' (\err -> return 'e')
-- 如果不匹配 则返回'e'

evalParser test (initParseState "*a")
-- Right 'e'
```



可以获取当前匹配的状态：

```haskell
satisfy :: MonadParser ParseException (ParseState String) m => (Char -> Bool) -> m Char
satisfy ck = do
    pState <- get --获取当前匹配状态
    case pState ^. inputState of
        c:cs -> 
            if ck c then 
                c <$ (put $ pState 
                    & inputState .~ cs 
                    & posState %~ updatePos c)
            else
                throwError $ Err ("err at " ++ show (pState ^. posState))  
        [] ->   throwError $ Err ("err at " ++ show (pState ^. posState) ++ " input exausted")
```



## Example

一个简单英语的语法解析器（就是一种old school的人工智能，符号分析）：

先定义词法和句法

```haskell
-- word
data Noun   = Noun String   deriving(Eq, Show)
data Verb   = Verb String   deriving(Eq, Show)
data Art    = Art String    deriving(Eq, Show)
data Prep   = Prep String   deriving(Eq, Show)
-- 名词动词冠词介词
```

短语：

```haskell
-- phrase
data NounPhrase 
    = SimpleNounPhrase Art Noun
    | NounPhrase NounPhrase PrepPhrase
    deriving(Eq, Show) -- 名词短语可能是简单的 冠词+名词 或者是 名词短语+介词短语
data PrepPhrase 
    = PrepPhrase Prep NounPhrase 
    deriving(Eq, Show)
data VerbPhrase 
    = SingleVerb Verb 
    | VerbPhrase VerbPhrase PrepPhrase
    deriving(Eq, Show)
```

句子：

```haskell
-- sentence
data Sentence
    = Sentence NounPhrase VerbPhrase
    deriving(Eq, Show)
```



然后定义解析器：

```haskell
noun :: MonadParser ParseException (ParseState String) m => m Noun
noun = Noun <$> token (some alpha)

verb :: MonadParser ParseException (ParseState String) m => m Verb
verb = Verb <$> token (some alpha)

art :: MonadParser ParseException (ParseState String) m => m Art
art = Art <$> choice (map (token . strg) ["a", "the"])

prep :: MonadParser ParseException (ParseState String) m => m Prep
prep = Prep <$> choice (map (token . strg) ["for", "to", "in", "on", "at", "by", "with"])

-------------------------- syntax ---------------------

-- noun-phrase = art noun | noun-phrase prep-phrase
-- noun-phrase = art noun noun-phrase'
nounPhrase :: MonadParser ParseException (ParseState String) m => m NounPhrase
nounPhrase = do
    a <- art
    n <- noun
    np' <- nounPhrase'
    case np' of
        Nothing -> return $ SimpleNounPhrase a n
        Just f  -> return $ f (SimpleNounPhrase a n)

-- noun-phrase' = prep-phrase noun-phrase' | epsi
nounPhrase' :: MonadParser ParseException (ParseState String) m => m (Maybe (NounPhrase -> NounPhrase))
nounPhrase' = do
    pp <- prepPhrase
    np' <- nounPhrase'
    case np' of
        Nothing -> return $ Just (\np -> NounPhrase np pp)
        Just f  -> return $ Just (\np -> f (NounPhrase np pp))
    <|>
    return Nothing

-- prep-phrase = prep noun-phrase
prepPhrase :: MonadParser ParseException (ParseState String) m => m PrepPhrase
prepPhrase = PrepPhrase <$> prep <*> nounPhrase

-- verb-phrase = verb-phrase prep-phrase | verb
-- verb-phrase = verb verb-phrase'
verbPhrase :: MonadParser ParseException (ParseState String) m => m VerbPhrase
verbPhrase = do
    v <- verb
    vp' <- verbPhrase'
    case vp' of
        Nothing -> return $ SingleVerb v
        Just f  -> return (f (SingleVerb v))

-- verb-phrase' = prep-phrase verb-phrase' | epsi
verbPhrase' :: MonadParser ParseException (ParseState String) m => m (Maybe (VerbPhrase -> VerbPhrase))
verbPhrase' = do
    pp <- prepPhrase
    vp' <- verbPhrase'
    case vp' of
        Nothing -> return $ Just (\vp -> VerbPhrase vp pp)
        Just f  -> return $ Just (\vp -> f (VerbPhrase vp pp))
    <|>
    return Nothing

sentence :: MonadParser ParseException (ParseState String) m => m Sentence
sentence = Sentence <$> nounPhrase <*> verbPhrase <* eof
```

基本和定义的语法一致，但需要处理一下左递归文法。



运行在应用中，我们选择可以打印出分析出来的语法结构：

![1561480106584](C:\Users\d5846\AppData\Roaming\Typora\typora-user-images\1561480106584.png)



## 用到的技术

- **parser combinators**

  ​	是一种解析器的抽象，提供了将多个解析器组合起来的技术，可以以一种接近BNF范式语法的方式编写解析器。

  ​	与之相对应的是parser generator，相对而言parser combinator的优点是可组合，可扩展性，而parser generator只能一次性生成一个解析器。但是一般的parser combinator使用的是**递归下降法解析**，会遇到左递归的情况（可以稍微修改算法，比如限制[递归深度](http://cs.uwindsor.ca/~hafiz/pub/p46-frost.pdf)）；而且parser combinator不能像generator那样通过分析语法结构进行优化，所以性能也会比较差（但是也可以改成parser generator combinator，将多个生成器组合成一个）。

  

  ​	基本原理：

  ```haskell
  type Parser a = String -> a
  ```

  ​	解析器可以看做一个函数，输入一个字符串，输出解析结果a，比如一个将字符串解析为浮点数的解析器可以描述为`float :: Parser Double`。但这个解析器只能用一次，因为它把解析的状态信息给丢弃了。

  ​	稍作修改：

  ```haskell
  type Parser a = String -> (a, String)
  --                              ^ 解析一个之后的字符串
  ```

  ​	现在具有此类型的解析器就可以进行组合了——只需要将解析后的状态传递给下一个解析器就可以了，可以做顺序解析：

  ```haskell
  (>>) :: Parser a -> Parser b -> Parser b -- 丢弃第一个解析器解析结果，只保留状态
  p >> q = \str -> 
      let (_, resStr) = p str -- 将第一次解析器的的状态拿出来
      in q resStr -- 传递给下一个解析器
  ```

  ​	这就是我们得到的第一个组合子，顺序匹配组合子。如果我们不丢弃上一个解析器的结果，我们就可以做上下文的匹配了。

  ​	现在的组合子，只能表示恰好有一种解析结果的的情况，然而大多数情况会又“没有匹配”或者“多个匹配”的情况。再次对解析器稍作修改：

  ```haskell
  type Parser a = String -> [(a, String)] -- 将所有解析的情况列举出来
  ```

  ​	现在具有此类型的解析器就能解析多种情况了：

  ```haskell
  (<|>) :: Parser a -> Parser a -> Parser a
  p <|> q = \str -> p str ++ q str -- 将p解释的结果和q解释的结果合并起来
  ```

  ​	这就是我们得到的组合子，选择匹配组合子。

  ​	当然，我们解析未必是字符串，或者说，我们可能还需要到其它的解析时的状态。继续修改解析器的类型：

  ```haskell
  type Parser s a = s -> [(a, s)]
  ```

  ​	现在的解析器的类型的形式，其实就是**将解析器描述成一个DFA了**：<u>一个解析器是DFA的一个节点，输入的s，表示解析的当前的状态；列表中的s，就是一些可能会到达的状态。</u>

  

  ​	但是用组合子来组合解析器的话，会具有很好的代数性质，可以在数学上保证解析器组合子的可扩展性，安全性，组合性等等性质：

  ```haskell
  epsi :: Parser s a
  epsi = \str -> [] -- 这个组合子什么都不做，可以用于占位
  
  -- <|>和epsi与其它解析器，可以组成一个幺半群：
  -- 幺元：p <|> epsi == epsi <|> p == p
  -- 结合律：(p <|> q) <|> r = p <|> (q <|> r)
  
  -- (>>)对(<|>)还有分配律。
  
  -- 还有很多组合子都会拥有各种各样的代数性质。
  ```

  ​	当然，这只是实现parser combinator的最简单的功能，我们还可以再实现其它功能。只不过，现在修改的话，需要在源程序中修改代码，不符合软件工程的基本规范。如何解决这个问题，还得用到下面的一些概念。

  

- **monad**

  ​	[单子](https://ncatlab.org/nlab/show/monad)，是一种出自于范畴论的一种概念：

  > 自函子范畴上的幺半群。

  在haskell代码里可以抽象成一个typeclass：

  ```haskell
  class Functor m => Monad m where 
  -- m先是一个函子，有fmap :: (a -> b) -> (m a -> m b)
  	pure :: a -> m a 
  	-- * -> m 单位元
  	join :: m (m a) -> m a 
  	-- m . m -> m 二元运算
  	
  	(>>=) :: m a -> (a -> m b) -> m b
  	(>>=) = join (\x -> x)
  	
  	(>>) :: m a -> m b -> m b
  	ma >> mb = ma >>= (\_ -> mb)
  	
  -- 需要满足：
  -- 单位元性质：
  -- join . pure == join . fmap pure == pure
  -- (左乘单位元)     (右乘单位元)        (单位元)
  -- 结合律：
  -- join . join == join . fmap join
  ```

  ​	但是我们不谈这个抽象有什么用，*有时候抽象就是我们的目的*。

  ​	对于parser combinator(下称parserc)来说，同样可以构造出这两个组合子出来。也就说明，parserc也是monad，并且实现该typeclass。

  

  ​	我们观察一下刚刚构造parser的过程中比较重要的几个形式：

  1. `r->a`可以从外部读取信息，并返回结果

     ```haskell
     type Reader r a = r -> a 
     --                ^ 可以代表外部的一个环境
     ```

     ​	这个可以实现为monad，并构造一个原语`local :: Reader r r`，即读出环境的值。

     ```haskell
     local >>= \r1 -> -- 读取环境值传递给r1
     local >>= \r2 -> -- 读取环境值传递给r2
     pure (r1 + r2) -- 返回相加的结果。不过在这里两个环境值都是一样的
     ```

     ​	reader monad一般用于**依赖注入**。

     

  2. `s->(a, s)`可以在返回结果的同时保存一些状态

     ```haskell
     type State s a = s -> (a, s)
     ```

     ​	这个同样也可以实现为一个monad，并构造两个原语`get :: State s s`，`put :: s -> State s a`。也就是获取当前状态，设置当前状态。

     ```haskell
     get >>= \s -> -- 获取当前状态
     put (s+1) >> -- 重新设置状态为s+1
     pure s -- 返回之前的状态。有点像s++的感觉
     ```

     ​	state monad一般用于描述**带状态**的计算。

  3. `[a]`列表可以表示所有可能的结果

     ​	同样可以实现为一个monad，带上`guard ::Bool -> [()]`。可以用于不确定性的计算。

     ```haskell
     ls >>= \a -> 
     guard (a>=3) >> -- 过滤掉列表中大于3的结果
     pure a
     ```

  

  ​	等等，还有很多很多形式monad，甚至还有其它形式的抽象，不一一列举，但是抽象成monad之后，不同的monad可以通用一套接口，但是可以*通过用组合的方式精确控制代码的行为*。比如基础的parserc就是一个state monad + list monad。

  ​	但现在每组合一个monad都要重新实现一次，这也没有做到代码的复用，接下来就是研究如何来组合monad。

  

- **monad transfermer**

  ​	monad的组合也有固定的模式，就是monad transformer。

  ​	比如State monad如果`m`是monad的话，则`s -> m (a, s)`关于a也是monad，这样就可以抽象出第一个monad transformer：

  ```haskell
  type StateT m s a = s -> m (a, s)
  ```

  ​	这时候我们只需要对`StateT m s`实现monad，再传入其它的monad，我们就可以得到其它拥有其它性质的monad了。

  ​	同样的也有：

  ```haskell
  type ListT m a = m [a] -- 不过遗憾的是，这个在很多情况下不满足monad定律
  type ExceptT m e a = m (Either e a) -- 用来表示有异常的情况
  type WriterT m w a = m (a, w) -- 用来打Log
  type ReaderT m r a = r -> m a -- 用来依赖注入
  -- 在这种情况下，其实State monad就是writer和reader的组合
  type ContT m r a = (a -> m r) -> m r -- cps，用来控制控制流
  
  type Identity a = a -- 一个用来占位的monad
  ```

  ​	这样，我们就可以组合得到不同行为的monad了。

  ```haskell
  type Parser = StateT (ListT Identity) -- 一个原始功能的parsec
  type Parser e = StateT (ExceptT (ListT Identity) e) -- 一个带异常的parserc
  type Parser r e = StateT (ExceptT (ListT (ContT Identity r)) e) -- 一个带回溯功能的parserc
  -- ...
  ```

  ​	但是，monad transformer仍然不完美。首先代码中会附带很多的`lift`，一个用来提升低层monad到高层使用的函数，会带来很多心智负担；其次，比如上面的parserc还是依赖具体的monad，如果要加上新的功能，还得重新编写一样的代码；同时很多时候不需要用到这么多的行为，冗余嵌套的monad会传递多余的上下文，造成太多不必要的性能损耗。

  ​	我们还可以进一步对monad transformer进行抽象。

  

- **mtl**

  ​	这是一个对monad transformer抽象的一个[库](https://hackage.haskell.org/package/mtl)。它对一些常用的monad transformer进行了抽象。比如说State monad transformer抽象为一个MonadState s m的typeclass：

  ```haskell
  doSth :: MonadState s m => m a -- m就是一个带状态s的monad，但是m是抽象的
  doSth2 :: (MonadState s1 m, MonadState s2 m) => -- m是一个带状态是s1, s2的monad
  ```

  ​	有了mtl库我们就可以随心组合monad了：

  ```haskell
  doSth :: (MonadError e m, MonadState s m, MonadCont r m) => m a 
  -- m可以抛出类型为e异常，可以保存类型为s的状态，并且可以随意控制控制流
  ```

  ​	那么我们就可以抽象出基础parserc的一个最基本的约束了：

  ```haskell
  type MonadParser e s m = (MonadError e m, MonadState s m)
  
  -- 若一个解析器内需要回溯功能，可以这样写：
  p :: (MonadParser e s m, MonadCont r m) => m Char
  -- 若要保存其它状态
  q :: (MonadParser e s1 m, MonadState s2 m) => m Char
  ```

  ​	

  ​	至此，我们就比较完美地提出了一套*extensible-effects*的方案了。这也是一套设计模式层面的解决方案。

  ​	（不过最近几年又提出了新的理论）

  

- **lens**

  ​	这是一个访问/修改复杂结构的[库](https://hackage.haskell.org/package/lens)。

  ​	在编写mtlParser的时候，我们自定义了一个类型来记录解析状态：

  ```haskell
  data Pos = Pos 
      {   line   :: Int
      ,   column :: Int
      }   deriving(Eq, Show)
      
  data ParseState s = ParseState 
      {   inputState  :: s
      ,   posState    :: Pos
      }   deriving(Eq, Show)
  
  ```

  ​	我们有访问/修改这个结构的需求。但是在Haskell中，只提供了「模式匹配」的语法来访问/修改数据结构。于是处理深层的数据就成为了老大难的问题。

  ​	或者说在纯函数式的数据结构里，因为数据的不变性（<u>可以保证你对程序的一些assumption不被副作用给破坏掉，这很重要</u>），访问修改的操作会比较困难。

  

  ​	**Lens对象就是getter和setter的抽象，能以一种可组合的方式，访问数据结构**。

  ​	其数学背景也是范畴论中一个概念：

  > Lens是一个Coalgebra for the CoState Comonad

  ​	比如对于mtlParser那两个数据结构：

  ```haskell
  lineLens :: Lens Pos Int -- 这是一个Lens，从Pos中可以get/set Int
  columnLens :: Lens Pos Int
  inputStateLens :: Lens (ParseState s) s
  posStateLens :: Lens (ParseState s) Pos
  
  view :: Lens a b -> a -> b -- getter
  set :: Lens a b -> b -> a -> b -- setter
  
  test = ParseState {
  	inputState = ("abcd", (1, 2)),
  	posState = Pos {
  		line = 0,
  		column = 4
  	}
  }
  
  view posStateLens test 
  -- Pos { line = 0, column = 4 }
  set posStateLens (Pos { line = 1, column = 4 }) test
  -- ParseState {
  --	inputState = ("abcd", (1, 2)),
  --	posState = Pos {
  --		line = 1,
  --		column = 4
  --	}
  -- }
  
  ```

  ​	我们还可以进行组合：

  ```haskell
  
  innerLens1 :: Lens (ParseState s) Int -- 构造了一个从ParseState s到Int的Lens
  innerLens1 = posStateLens . lineLens 
  --             访问pos      访问line
  
  view innerLens1 test 
  -- 0
  set innerLens1 1 test
  -- ParseState {
  --	inputState = ("abcd", (1, 2)),
  --	posState = Pos {
  --		line = 1,
  --		column = 4
  --	}
  -- }
  
  view (inputStateLens . _2 . _1) test 
  -- 2
  ```

  ​	

  ​	其本质上就是一个这样的数据结构：

  ```haskell
  type Lens a b = a -> (b->a, b)
  -- 可以提取出来：
  -- a -> b就是getter
  -- a -> (b -> a) 就是一个setter
  
  -- 组合
  compose :: Lens c b -> Lens b a -> Lens c a 
  compose lens1 lens2 = 
  	let (bc, b) = cbl c in
      let (ab, a) = bal b in 
      (bc . ab, a) 
  ```

  ​	还能更复杂一点的：

  ```haskell
  type Lens s t a b = s -> (b -> t, a)
  -- getter :: s -> a
  -- setter :: s -> b -> t
  -- 因为Lens是抽象的getter和setter，所以不需要知道原数据里的东西具体是什么类型
  ```

  ​	

  ​	作者提供的Lens库中，Lens具有类型：

  ```haskell
  type Lens s t a b = forall f. Functor f. (a -> f b) -> (s -> f t)
  ```

  ​	这是对原来的版本做了一个变换，具体可以用**[Yoneda Lemma](https://ncatlab.org/nlab/show/Yoneda+lemma)**可以证明。但是这种形式更为抽象，<u>可以选择不同的f对lens的行为进行更精确的控制</u>，并且可以达到更好的性能。

  ​	我自己也写过一篇文章来介绍lens：<https://zhuanlan.zhihu.com/p/58868310>，这里就不接着深入了。

  ​	

  ​	当然这个Lens概念不仅仅适合于Haskell这一门语言，它就是一个通用的数据结构，在对应的场景就能起到作用。比如js的Rambda库有lens；在java中也能使用lens，为一个类添加方法（因为java中的方法不是first-class的，不能随意创建/定义）。

  

- **continuation passing style(CPS)**

  ​	这是一种将控制流(**continuation**)封装在对象中的技术。在数学上，CPS这种形式对应的是**直觉主义逻辑**与**经典逻辑**之间的变换（**Gödel–Gentzen 转换**）。

  ​	一般在Functional Programming中，控制流是隐式的，没有控制流语句。而我们在parserc中有控制控制流的需求，比如说回溯。这时候就需要将CPS变换了（上面用了ContMonad去封装）。

  ​	其实CPS变换的含义也不复杂（只是需要去证明而已，但是是non-trivial的），和JavaScript中的callback的概念其实是相似的——将一个回调函数作为参数传递到另一个函数中，作为*即将*被运行的函数——<u>这个回调参数，其实就是一个控制流(continuation)</u>，它表示着即将能被执行的代码（类似程序计数器的作用）。封装起来就是ContMonad的形式：

  ```haskell
  type Cont r a = (a -> r) -> r
  --                  ^ continuation/callback
  ```

  ​	这里就不再深入了。

...



## 总结

​	这个“项目”其实代码不是特别复杂，但是是**函数式编程**应用的一个比较好的实现。

​	函数式编程是一个非常有意义的“编程范式”，相对于面向对象、面向过程来说，函数式就是一种面向函数，面向各种抽象的编程范式。对于软件开发也十分有指导意义。事实上，函数式编程近年来在各种项目中有不少影子，java8，c++中的lambda等等，新世代的编程语言也受到了影响swift/rust等等。

​	函数式编程门道还很深，是一个很有意义的研究方向。