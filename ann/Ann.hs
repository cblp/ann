{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

module Ann (Annotations, type (//), typeAnnotations) where

import           Language.Haskell.TH (Type (AppT, ConT))

type t // ann = t
infixl 0 //

type Annotations = [Type]

typeAnnotations :: Type -> Annotations
typeAnnotations = \case
  AppT (AppT (ConT conName) nestedType) ann | conName == ''(//) ->
    ann : typeAnnotations nestedType
  _ -> []
