{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

import           Ann (type (//))
import           Ann.Aeson (deriveToJson)
import qualified Ann.Aeson as JSON (Ignore, Name)
import           Ann.Show (deriveShow)
import qualified Ann.Show as Show (Content)
import           Data.Aeson (toJSON)
import           Data.Aeson.QQ.Simple (aesonQQ)
import           Test.Tasty (defaultMain, testGroup)
import           Test.Tasty.HUnit (testCase, (@?=))

data MyRecord = MyRecord
  { foo :: (Integer -> String) // JSON.Ignore // Show.Content "<function>"
  , bar :: String // JSON.Name "BAR"
  , baz :: [Integer]
  }
deriveShow   ''MyRecord
deriveToJson ''MyRecord

main :: IO ()
main =
  defaultMain $
    testGroup
      "Ann"
      [ testCase "JSON.encode" $
        toJSON example @?= [aesonQQ|{"BAR": "Blue Oyster", "baz": [3, 15]}|]
      , testCase "show" $
          show example
          @?= "MyRecord{foo = <function>, bar = \"Blue Oyster\", baz = [3,15]}"
      ]
  where
    example = MyRecord{foo = show, bar = "Blue Oyster", baz = [3, 15]}
