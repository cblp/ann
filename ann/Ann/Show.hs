{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TemplateHaskell #-}

module Ann.Show (Content, deriveShow) where

import           Ann (Annotations, typeAnnotations)
import           Data.Foldable (fold)
import           Data.List (intersperse)
import           Data.Maybe (listToMaybe)
import           Data.Traversable (for)
import           GHC.TypeLits (Symbol)
import           Language.Haskell.TH (Dec, Q, TyLit (StrTyLit),
                                      Type (AppT, ConT, LitT), conT, fieldPat,
                                      nameBase, newName, recP, stringE, varE,
                                      varP)
import qualified Language.Haskell.TH as TH
import           Language.Haskell.TH.Datatype (ConstructorInfo (..), ConstructorVariant (RecordConstructor),
                                               DatatypeInfo (..), reifyDatatype)

data Content (s :: Symbol)

lookupContent :: Annotations -> Maybe String
lookupContent annotations =
  listToMaybe
    [ content
    | AppT (ConT con) (LitT (StrTyLit content)) <- annotations, con == ''Content
    ]

data Field = Field
  { name    :: TH.Name
  , varName :: TH.Name
  , content :: Maybe String
  }

deriveShow :: TH.Name -> Q [Dec]
deriveShow recordName =
  do
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
        let content     = lookupContent annotations
        pure Field{name, varName, content}
    let
      fieldsPat =
        recP
          constructorName
          [ fieldPat name $ varP varName
          | Field{name, varName, content = Nothing} <- fields
          ]
    let
      impl =
        foldr1 append $
              stringE (nameBase constructorName ++ "{")
          :   intercalate
                [stringE ", "]
                [ [ stringE $ nameBase name ++ " = "
                  , maybe [| show $(varE varName) |] stringE content
                  ]
                | Field{name, varName, content} <- fields
                ]
          ++  [stringE "}"]
    [d|
      instance Show $(conT recordName) where
        show $fieldsPat = $impl
      |]
  where
    append x y = [| $x ++ $y |]

intercalate :: Monoid m => m -> [m] -> m
intercalate x xs = fold $ intersperse x xs
