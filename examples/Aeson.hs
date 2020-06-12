{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

import           Ann (type (//))
import           Ann.Aeson (deriveToJson)
import qualified Ann.Aeson as JSON (Ignore, Name)
import qualified Ann.Show as Show (Content)
import           Data.Aeson (toJSON)
import           Data.Aeson.QQ.Simple (aesonQQ)
import           Test.Tasty (defaultMain)
import           Test.Tasty.HUnit (testCase, (@?=))

data ExampleRecord = ExampleRecord
  { foo :: (Integer -> String) // JSON.Ignore // Show.Content "<function>"
  , bar :: String // JSON.Name "BAR"
  , baz :: [Integer]
  }

deriveToJson ''ExampleRecord

main :: IO ()
main =
  defaultMain $
  testCase "JSON.encode" $
    toJSON ExampleRecord{foo = show, bar = "Blue Oyster", baz = [3, 15, 9, 20]}
    @?= [aesonQQ|{"BAR": "Blue Oyster", "baz": [3, 15, 9, 20]}|]
