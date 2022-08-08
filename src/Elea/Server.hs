-- | ...
-- Module: Server
-- Description : Computer servers
--
-- Describe this module more here.
-- ...

{-# LANGUAGE DeriveGeneric #-}

module Elea.Server (
    Server (..)
  , Kind (..)
  ) where


import            Data.Aeson (
    FromJSON (..), (.:)
  , ToJSON (..), (.=)
  )
import           Data.Aeson.Types (prependFailure, typeMismatch)
import qualified Data.Aeson as JSON (
    object, withObject
  , Value (String)
  )
import           Data.Text (Text)
import           GHC.Generics



data Kind = 
    Internet
  | EleaClient
  deriving (Show)

data Server = Server {
    name :: ServerName
  , uri  :: Text
}

instance FromJSON Server where
  parseJSON = JSON.withObject "Server" $ \v -> Server
    <$> v .: "name"
    <*> v .: "uri"

instance ToJSON Server where
  toJSON server = JSON.object [
      "name" .= server.name
    , "uri"  .= server.uri
    ]



newtype ServerName = ServerName {
  getServerName :: Text
} deriving (Generic, Show)

instance FromJSON ServerName where
  parseJSON (JSON.String t) = return $ ServerName t
  parseJSON invalid         = 
    prependFailure "parsing ServerName failed, "
            (typeMismatch "String" invalid)

instance ToJSON ServerName

