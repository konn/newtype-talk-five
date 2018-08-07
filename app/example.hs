{-# LANGUAGE DataKinds, DeriveGeneric, DerivingVia, FlexibleInstances #-}
{-# LANGUAGE TypeOperators, MultiParamTypeClasses                     #-}

module Main where
import           Data.Aeson
import           Data.Aeson.Generic.DerivingVia
import qualified Data.ByteString.Lazy.Char8     as LBS
import           Data.Reflection
import           GHC.Generics
import           Data.DerivingIso
import           Data.Semigroup (Last(..), First(..), Dual(..))

data MyConfig = MyConfig { mcNameOfProcess :: String
                         , mcArgsToProcess :: [String]
                         }
  deriving (Read, Show, Eq, Ord, Generic)
  deriving (ToJSON, FromJSON)
       via WithOptions '[ FieldLabelModifier     '[CamelTo2 "_" , Drop 2]
                        , ConstructorTagModifier '[CamelTo2 "_"]
                        ]
                        MyConfig
  deriving (Semigroup)
       via MyConfig `SameRepAs` (Last String, [String])

data Init
instance Reifies Init (String -> String) where
  reflect _ = init

data OtherConfig = OtherConfig { otrNameOfProcess :: Maybe String
                               , otrArgsToProcess :: [String]
                               }
  deriving (Read, Show, Eq, Ord, Generic)
  deriving (ToJSON, FromJSON)
       via WithOptions '[ FieldLabelModifier     '[CamelTo2 "-"]
                        , ConstructorTagModifier '[CamelTo2 "-", UserDefined Init]
                        , SumEnc                  TwoElemArr
                        , TagSingleConstructors  'True
                        , OmitNothingFields      'True
                        ]
                        OtherConfig

main :: IO ()
main = do
  LBS.putStrLn $ encode $ MyConfig "foo" ["bar", "baz"]
  LBS.putStrLn $ encode $ OtherConfig Nothing ["bar", "baz"]

{-
ghci> main
{"args_to_process":["bar","baz"],"name_of_process":"foo"}
["other-config",{"otr-args-to-process":["bar","baz"]}]
-}
