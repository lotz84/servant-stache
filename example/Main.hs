{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

import GHC.Generics

import Data.Aeson
import Network.HTTP.Media ((//))
import qualified Network.Wai.Handler.Warp as Warp
import Servant
import Servant.Mustache


-- * For HTML Examle

data User = User
  { name :: String
  , age :: Int
  } deriving (Generic, ToJSON)


-- * For Template Example

data CSS

instance Accept CSS where
  contentType _ = "text" // "css"

data CSSData = CSSData
  { darken :: Bool
  , pageWidth :: Int
  } deriving (Generic, ToJSON)


-- * Server implementation

type API = Get '[HTML "user"] User
      :<|> "style.css" :> Get '[Template CSS "style"] CSSData

api :: Proxy API
api = Proxy

server :: Server API
server = pure (User "lambdabot" 35)
    :<|> pure (CSSData True 400)

main :: IO ()
main = do
  loadTemplates "example/templates"
  putStrLn "Listening on port 8080"
  Warp.run 8080 $ serve api server