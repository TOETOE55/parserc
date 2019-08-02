{-# LANGUAGE TemplateHaskell, TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveFunctor #-}

module Comp where

import Data.Comp
import Data.Comp.Derive
import Data.Comp.Show ()
import Data.Comp.Equality ()

-- Signature for values and operators
data Value a = Const Int | Pair a a
  deriving Functor
data Op a    = Add a a | Mult a a | Fst a | Snd a
  deriving Functor

-- Signature for the simple expression language
type Sig = Op :+: Value

-- Derive boilerplate code using Template Haskell
$(derive [makeTraversable, makeFoldable,
          makeEqF, makeShowF, smartConstructors, smartAConstructors]
         [''Value, ''Op])