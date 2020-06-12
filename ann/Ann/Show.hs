{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}

module Ann.Show (Content) where

import           GHC.TypeLits (Symbol)

data Content (s :: Symbol)
