{-#LANGUAGE ConstraintKinds
, FlexibleContexts
, FlexibleInstances
, MultiParamTypeClasses
, UndecidableInstances
, GeneralizedNewtypeDeriving
, RankNTypes
, TemplateHaskell#-}

module Parser where

import Control.Monad.State
import Control.Monad.Except
import Control.Monad.Cont
import Control.Monad.Identity
import Control.Applicative
import Control.Lens
import Data.Char

type MonadParser e s m = (Alternative m, MonadState s m, MonadError e m, MonadPlus m) 
-- type MonadParser e s m = (Alternative m, MonadState s m, MonadError e m) 
data Pos = Pos 
    {   _line   :: Int
    ,   _column :: Int
    }   deriving(Eq, Show)

makeLenses ''Pos

data ParseState s = ParseState 
    {   _inputState  :: s
    ,   _posState    :: Pos
    }   deriving(Eq, Show)

makeLenses ''ParseState

newtype ParseException = Err String deriving(Eq, Show, Semigroup, Monoid)

type ParserT r e s m = StateT (ParseState s) (ExceptT e (ContT r m))
type Parser e s a = ParserT (Either e (a, ParseState s)) e s Identity a
-- type ParserT e s m = StateT (ParseState s) (ExceptT e m)
-- type Parser e s = ParserT e s Identity

updatePos :: Char -> Pos -> Pos
updatePos char pos = case char of
    '\n'    -> pos & line   %~ (+1)
    '\t'    -> pos & column %~ (\c -> c+8 - (c-1) `mod` 8)
    _       -> pos & column %~ (+1) 

-------------------------------------------------------------------

initParseState :: s -> ParseState s
initParseState s = ParseState s (Pos 0 0)


runParserT :: Monad m => 
    ParserT r e s m a -> 
    ParseState s -> 
    (Either e (a, ParseState s) -> m r) 
    -> m r
runParserT p s cont = runContT (runExceptT (runStateT p s)) cont

evalParserT :: Monad m => 
    ParserT r e s m a -> 
    ParseState s -> 
    (Either e a -> m r) 
    -> m r
evalParserT p s cont = runParserT p s (cont . fmap fst)


execParserT :: Monad m => 
    ParserT r e s m a -> 
    ParseState s -> 
    (Either e (ParseState s) -> m r) 
    -> m r
execParserT p s cont = runParserT p s (cont . fmap snd)

runParser ::  Parser e s a -> ParseState s -> Either e (a, ParseState s)
runParser p s = runIdentity (runParserT p s return)

evalParser ::  Parser e s a -> ParseState s -> Either e a
evalParser p s = liftM fst (runParser p s)

execParser ::  Parser e s a -> ParseState s -> Either e (ParseState s)
execParser p s = liftM snd (runParser p s)

{-
runParserT :: Monad m => 
    ParserT e s m a -> 
    ParseState s -> 
    m (Either e (a, ParseState s))
runParserT p = runExceptT . runStateT p 

runParser ::  Parser e s a -> ParseState s -> Either e (a, ParseState s)
runParser p = runIdentity . runParserT p 

-}
------------------------------------ some tokenizer and parserc ------------------------------------------------
choice :: MonadParser e s m => [m a] -> m a
choice ps = foldr (<|>) mzero ps


eof :: MonadParser ParseException (ParseState String) m => m ()
eof = do
    pState <- get
    if null (pState ^. inputState) then
        return ()
    else
        throwError $ Err ("err at " ++  show (pState ^. posState))

satisfy :: MonadParser ParseException (ParseState String) m => (Char -> Bool) -> m Char
satisfy ck = do
    pState <- get
    case pState ^. inputState of
        c:cs -> 
            if ck c then 
                c <$ (put $ pState 
                    & inputState .~ cs 
                    & posState %~ updatePos c)
            else
                throwError $ Err ("err at " ++ show (pState ^. posState))  
        [] ->   throwError $ Err ("err at " ++ show (pState ^. posState) ++ " input exausted")


char :: MonadParser ParseException (ParseState String) m => Char -> m Char
char = satisfy . (==)

strg :: MonadParser ParseException (ParseState String) m => String -> m String
strg (c:cs) = (:) <$> char c <*> strg cs
strg [] = return []

alpha :: MonadParser ParseException (ParseState String) m => m Char
alpha = satisfy isAlpha

upper :: MonadParser ParseException (ParseState String) m => m Char
upper = satisfy isUpper

lower :: MonadParser ParseException (ParseState String) m => m Char
lower = satisfy isLower

space :: MonadParser ParseException (ParseState String) m => m Char
space = satisfy isSpace

digit :: MonadParser ParseException (ParseState String) m => m Char
digit = satisfy isDigit

blank :: MonadParser ParseException (ParseState String) m => m [Char]
blank = some space

token :: MonadParser ParseException (ParseState String) m => m a -> m a
token p = let sp = many space in sp *> p <* sp

int :: MonadParser ParseException (ParseState String) m => m Integer
int = token $ read <$> some digit

float :: MonadParser ParseException (ParseState String) m => m Double
float = aDotb 
    <|> fromInteger <$> int
    where
        aDotb = token $ do
            let sd = some digit
            integer <- sd
            char '.'
            frac <- sd
            return (read (integer ++ '.':frac) :: Double)

paren :: MonadParser ParseException (ParseState String) m => m a -> m a
paren p = char '(' *> p <* char ')'

bracket :: MonadParser ParseException (ParseState String) m => m a -> m a
bracket p = char '[' *> p <* char ']'

brace :: MonadParser ParseException (ParseState String) m => m a -> m a
brace p = char '{' *> p <* char '}'

-- (float) or [float] or {float}
example1 :: MonadParser ParseException (ParseState String) m => m Double
example1 = 
    paren float <|> bracket float <|> brace float

test :: Int
test = 1