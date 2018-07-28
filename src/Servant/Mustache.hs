{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Servant.Mustache
-- Copyright   :  (c) Tatsuya Hirose 2018
-- Maintainer  :  tatsuya.hirose.0804@gmail.com
-- Stability   :  experimental
--
-- Rendering Mustache templates with <http://hackage.haskell.org/package/servant servant>
-- using <https://hackage.haskell.org/package/stache stache>. This package is heavily
-- inspired by <http://hackage.haskell.org/package/servant-ede servant-ede>.
--
-- This package provides two Content-Types with servant (i.e just like 'JSON'),
-- 'HTML' and 'Template'.
-- 
--
-- - 'HTML' takes a filename as parameter and lets you render the template
--   with that name against the data returned by a request handler using
--   the @text\/html;charset=utf-8@ MIME type, XSS-sanitizing the said data
--   along the way.
-- - 'Template' does the same except that it's parametrized over the content type
--   to be sent along with the rendered template. Any type that has an 'Accept'
--   instance will do.
-----------------------------------------------------------------------------
module Servant.Mustache
  ( -- * Content-Types
    HTML
  , Template

    -- * Loading template files
  , loadTemplates
) where

import Data.Proxy (Proxy(..))
import Control.Concurrent.MVar (MVar, newEmptyMVar, readMVar, putMVar)
import GHC.TypeLits (KnownSymbol(..), Symbol, symbolVal)
import System.IO.Unsafe (unsafePerformIO)

import Control.Monad.IO.Class (MonadIO(..))
import Data.Aeson
import qualified Data.HashMap.Strict as HM
import Data.Text (Text, pack)
import Data.Text.Lazy.Encoding (encodeUtf8)
import qualified Data.Vector as V
import Network.HTTP.Media ((//), (/:))
import Servant.API.ContentTypes (Accept(..), MimeRender(..))
import Text.HTML.SanitizeXSS (sanitize)
import qualified Text.Mustache as Stache


-- global store of templates
{-# NOINLINE __template_store #-}
__template_store :: MVar Stache.Template
__template_store = unsafePerformIO newEmptyMVar


-- | This function initializes a global template store
--   and fills it with the resulting compiled templates.
--   This function /does not/ scan the directory recursively.
--
--   /IMPORTANT/: Must /always/ be called before starting your /servant/ application.
-- 
loadTemplates :: (MonadIO io) => FilePath -> io ()
loadTemplates dir = do
  template <- Stache.compileMustacheDir undefined dir
  liftIO $ putMVar __template_store template


-- | A generic template for any content, parametrized over the
--   content-type (or MIME) associated to the template.
--
--   The first parameter is the content-type you want to send along with
--   rendered templates (must be an instance of 'Accept'). The second
--   parameter is the name of (or path to) the template file. Any type
--   used with this content-type (like @CSSData@ below) must have an
--   instance of the 'ToJSON' class.
--
--   Here is how you could render and serve, say, /CSS/
--   (Cascading Style Sheets) templates that make use
--   of some @CSSData@ data type to tweak the styling.
--
-- @
-- data CSS
--
-- instance Accept CSS where
--   contentType _ = "text" // "css"
--
-- data CSSData = CSSData
--   { darken :: Bool
--   , pageWidth :: Int
--   } deriving (Generic, ToJSON)
--
-- type StyleAPI = "style.css" :> Get '[Template CSS "style"] CSSData
-- @
--
-- @style.mustache@ could for example be:
--
-- > body {
-- >   {{#darken}}
-- >   background-color: #222;
-- >   color: blue;
-- >   {{/darken}}
-- >   {{^darken}}
-- >   background-color: white;
-- >   color: back;
-- >   {{/darken}}
-- > }
-- >
-- > #content {
-- >   width: {{pageWidth}}px;
-- >   margin: 0 auto;
-- > }
--
-- A complete, runnable version of this can be found
-- in the @examples@ folder of the git repository
data Template (ct :: *) (file :: Symbol)

-- | use @ct@'s content-type
instance Accept ct => Accept (Template ct file) where
  contentType _ = contentType ctproxy
    where ctproxy = Proxy :: Proxy ct

-- | render the contents of @file@ in the global template store
instance (KnownSymbol file, Accept ct, ToJSON a) => MimeRender (Template ct file) a where
  mimeRender _ val =
    encodeUtf8 $ Stache.renderMustache template (toJSON val)
    where template = tstore { Stache.templateActual = Stache.PName (pack filename) }
          filename = symbolVal (Proxy :: Proxy file)
          tstore = unsafePerformIO (readMVar __template_store)


-- | 'HTML' content type. 'HTML' takes a type-level string
--   which is a filename for the template you want to use
--   to render values. Types used with the 'HTML' content
--   type (like @User@ below) must provide a 'ToJSON' instance.
--
--   The filename doesn't contain any extension. For example, if
--   the template filename is @user.mustache@, `HTML` takes @user@
--   as filename.
--
-- @
-- data User = User { name :: String, age :: Int } deriving (Generic, ToJSON)
--
-- type UserAPI = "user" :> Get '[HTML "user"] User
--
-- userAPI :: Proxy UserAPI
-- userAPI = Proxy
--
-- server :: Server API
-- server = pure (User "lambdabot" 31)
--
-- main :: IO ()
-- main = do
--   loadTemplates "./templates"
--   run 8080 (serve userAPI server)
-- @
--
-- This will look for a template at @.\/templates\/user.mustache@, which could
-- for example be:
--
-- > <ul>
-- >   <li><strong>Name:</strong> {{ name }}</li>
-- >   <li><strong>Age:</strong> {{ age }}</li>
-- > </ul>
--
-- /IMPORTANT/: it XSS-sanitizes every bit of text in the 'Value'
-- passed to the template.
data HTML (file :: Symbol)

-- | @text/html;charset=utf-8@
instance Accept (HTML file) where
  contentType _ = "text" // "html" /: ("charset", "utf-8")

-- | XSS-sanitizes data before rendering it
instance (KnownSymbol file, ToJSON a) => MimeRender (HTML file) a where
  mimeRender _ val = mimeRender (Proxy :: Proxy (Template (HTML file) file)) $
    sanitizeValue (toJSON val)

sanitizeObject :: Object -> Object
sanitizeObject = HM.fromList . map sanitizeKV . HM.toList

sanitizeKV :: (Text, Value) -> (Text, Value)
sanitizeKV (k, v) = (sanitize k, sanitizeValue v)

sanitizeValue :: Value -> Value
sanitizeValue (String s) = String (sanitize s)
sanitizeValue (Array a) = Array (V.map sanitizeValue a)
sanitizeValue (Object o) = Object (sanitizeObject o)
sanitizeValue x = x
