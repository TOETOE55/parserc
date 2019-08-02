{-#LANGUAGE ConstraintKinds
, FlexibleContexts
, GeneralizedNewtypeDeriving
, FlexibleInstances
, UndecidableInstances#-}
module Syntax where
    
import Control.Applicative
import Control.Monad.Cont
import Control.Monad.Except
import Parser


import Prelude hiding (pred)

-- word
data Noun   = Noun String   deriving(Eq, Show)
data Verb   = Verb String   deriving(Eq, Show)
data Art    = Art String    deriving(Eq, Show)
data Prep   = Prep String   deriving(Eq, Show)

-- phrase
data NounPhrase 
    = SimpleNounPhrase Art Noun
    | NounPhrase NounPhrase PrepPhrase
    deriving(Eq, Show)
data PrepPhrase 
    = PrepPhrase Prep NounPhrase 
    deriving(Eq, Show)
data VerbPhrase 
    = SingleVerb Verb 
    | VerbPhrase VerbPhrase PrepPhrase
    deriving(Eq, Show)

-- sentence
data Sentence
    = Sentence NounPhrase VerbPhrase
    deriving(Eq, Show)

--------------------- implement -----------------------------

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

---------------------------- pretty print ---------------------------------

nounPrettyPrintHelper :: Int -> Noun -> String
nounPrettyPrintHelper n noun = 
    take (2*n) (repeat ' ') ++ "(" ++ show noun ++ ")"

verbPrettyPrintHelper :: Int -> Verb -> String
verbPrettyPrintHelper n verb = 
    take (2*n) (repeat ' ') ++ "(" ++ show verb ++ ")"

artPrettyPrintHelper :: Int -> Art -> String
artPrettyPrintHelper n art = 
    take (2*n) (repeat ' ') ++ "(" ++ show art ++ ")"

prepPrettyPrintHelper :: Int -> Prep -> String
prepPrettyPrintHelper n prep = 
    take (2*n) (repeat ' ') ++ "(" ++ show prep ++ ")"

nounPhrasePrettyPrintHelper :: Int -> NounPhrase -> String
nounPhrasePrettyPrintHelper n (SimpleNounPhrase art noun) = 
    take (2*n) (repeat ' ') ++ "(NounPhrase\n" ++
        artPrettyPrintHelper (n+1) art ++ "\n" ++
        nounPrettyPrintHelper (n+1) noun ++ ")"
nounPhrasePrettyPrintHelper n (NounPhrase np pp) = 
    take (2*n) (repeat ' ') ++ "(NounPhrase\n" ++
        nounPhrasePrettyPrintHelper (n+1) np ++ "\n" ++
        prepPhrasePrettyPrintHelper (n+1) pp ++ ")"

prepPhrasePrettyPrintHelper :: Int -> PrepPhrase -> String
prepPhrasePrettyPrintHelper n (PrepPhrase prep np) = 
    take (2*n) (repeat ' ') ++ "(PrepPhrase\n" ++
    prepPrettyPrintHelper (n+1) prep ++ "\n" ++
    nounPhrasePrettyPrintHelper (n+1) np ++ ")"

verbPhrasePrettyPrintHelper :: Int -> VerbPhrase -> String
verbPhrasePrettyPrintHelper n (SingleVerb v) = verbPrettyPrintHelper n v
verbPhrasePrettyPrintHelper n (VerbPhrase vp pp) = 
    take (2*n) (repeat ' ') ++ "(VerbPhrase\n" ++
    verbPhrasePrettyPrintHelper (n+1) vp ++ "\n" ++
    prepPhrasePrettyPrintHelper (n+1) pp ++ ")"

sentencePrettyPrint :: Sentence -> String
sentencePrettyPrint (Sentence np vp) = 
    "Sentence\n" ++ 
        nounPhrasePrettyPrintHelper 1 np ++ "\n" ++
        verbPhrasePrettyPrintHelper 1 vp

test :: Either ParseException Sentence
test = evalParser sentence (initParseState "the professor lectures to the student with the cat")

