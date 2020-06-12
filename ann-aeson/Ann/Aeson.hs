{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Annotations and tools for deriving Aeson instances
module Ann.Aeson (Ignore, Name, deriveToJson) where

import           Ann (Annotations, typeAnnotations)
import           Data.Aeson (ToJSON, object, toJSON, (.=))
import           Data.Maybe (fromMaybe, listToMaybe)
import qualified Data.Text as Text
import           Data.Traversable (for)
import           GHC.TypeLits (Symbol)
import           Language.Haskell.TH (Dec, Q, TyLit (StrTyLit),
                                      Type (AppT, ConT, LitT), conT, fieldPat,
                                      infixApp, listE, nameBase, newName, recP,
                                      stringE, varE, varP)
import qualified Language.Haskell.TH as TH
import           Language.Haskell.TH.Datatype (ConstructorInfo (..), ConstructorVariant (RecordConstructor),
                                               DatatypeInfo (..), reifyDatatype)
-- import           Language.Haskell.TH.Ppr (pprint)

-- | Ignore field during encoding and decoding
data Ignore

hasIgnore :: Annotations -> Bool
hasIgnore = elem $ ConT ''Ignore

-- | Set name for encoding and decoding
data Name (s :: Symbol)

lookupName :: Annotations -> Maybe String
lookupName annotations =
  listToMaybe
    [ name
    | AppT (ConT con) (LitT (StrTyLit name)) <- annotations, con == ''Name
    ]

data Field = Field
  { name        :: TH.Name
  , varName     :: TH.Name
  , annotations :: [Type]
  , jsonName    :: String
  }

deriveToJson :: TH.Name -> Q [Dec]
deriveToJson recordName = do
  DatatypeInfo{datatypeCons} <- reifyDatatype recordName
  [constructorInfo] <- pure datatypeCons
  let
    ConstructorInfo{constructorName, constructorVariant, constructorFields} =
      constructorInfo
  RecordConstructor fieldNames <- pure constructorVariant
  fields <-
    for (zip fieldNames constructorFields) $ \(name, type_) -> do
      varName <- newName $ nameBase name
      let annotations = typeAnnotations type_
      let jsonName = fromMaybe (nameBase name) $ lookupName annotations
      pure Field{name, varName, annotations, jsonName}
  let
    fieldsPat =
      recP
        constructorName
        [ fieldPat name $ varP varName
        | Field{name, varName, annotations} <- fields
        , not $ hasIgnore annotations
        ]
  let
    fieldPairs =
      listE
        [ infixApp
            [| Text.pack $(stringE jsonName) |]
            (varE '(.=))
            (varE varName)
        | Field{varName, annotations, jsonName} <- fields
        , not $ hasIgnore annotations
        ]
  [d|
    instance ToJSON $(conT recordName) where
      toJSON $fieldsPat = object $fieldPairs
    |]

-- [ AppT (ConT Ann.Show.Content) (LitT (StrTyLit "<function>"))
-- , ConT Ann.Aeson.Ignore
-- ]
