{-# LANGUAGE DeriveAnyClass #-}

module APIs where

import           Data.Text
import           GHC.Generics
import           Servant.API
import           Servant.Foreign     (Foreign, GenerateList, HasForeign,
                                      HasForeignType, Req, listFromAPI, typeFor,
                                      _reqReturnType)
import           Typescript

data User = User
  { userId        :: Int
  , userFirstName :: Text
  , userLastName  :: Text
  } deriving (Eq, Show, Generic, TypescriptType)

data Team = Team {
  -- | The players on the team
  players :: [Player],
  teamName :: Text
  } deriving (Eq, Show, Generic, TypescriptType)

data Player = Player {
  playerName :: Text,
  playerRank :: Int
  } deriving (Eq, Show, Generic, TypescriptType)

type API = "users" :> Get '[JSON] Text
      :<|> "hold" :> Get '[JSON] Text
