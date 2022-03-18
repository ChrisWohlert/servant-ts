{-# LANGUAGE DeriveAnyClass #-}

module Main where

import           ServantTS.Convert
import           Data.Maybe      (fromMaybe)
import           Data.Proxy
import           APIs
import Data.Text (Text)
import qualified Data.Text as T
import ServantTS.Output.TSFunctions
import           Servant.API
import           Servant.Foreign (Foreign, GenerateList, HasForeign,
                                  HasForeignType, Req, listFromAPI, typeFor,
                                  _reqReturnType)
import           Typescript
import Data.Text.Prettyprint.Doc
import ServantTS.Output.Docs
import ServantTS.Output.RequestFlavors.Fetch (Fetch)
import Dhall hiding (sequence)
import Data.Text


data Example =
  Example
    {decFile  :: Text
    ,funcFile :: Text
    } deriving (Generic, ToDhall)

instance FromDhall Example

main :: IO ()
main = do
  print $ show (apiToTypeDeclarationDoc asTS)
  genTypes "test.ts"
 where
  asTS         = servantToReqTS (Proxy :: Proxy Vanilla) (Proxy :: Proxy API)
  reqToTSFunction = defaultReqToTSFunction (Proxy @Fetch)
  config = Example
       {decFile = T.pack $ show (apiToTypeDeclarationDoc asTS)
       ,funcFile = T.pack $ show (apiToFunctionDoc asTS reqToTSFunction)
       }




genTypes path = do
    writeFile path . unpack $ Data.Text.unlines
      [ mkTypescriptDeclaration (Proxy :: Proxy Vanilla) (Proxy :: Proxy [User])
      , mkTypescriptDeclaration (Proxy :: Proxy Vanilla) (Proxy :: Proxy Team)
      , mkTypescriptDeclaration (Proxy :: Proxy Vanilla) (Proxy :: Proxy Player)
      ]