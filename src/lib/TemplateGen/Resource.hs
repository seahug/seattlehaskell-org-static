module TemplateGen.Resource (
    Resource(..)
  , fullUrl
) where

import qualified TemplateGen.Hash as H
import qualified TemplateGen.UrlString as UL

data Resource = Resource UL.UrlString (Maybe H.Hash) deriving Show

fullUrl :: Resource -> UL.UrlString
fullUrl (Resource url Nothing) = url
fullUrl (Resource url (Just (H.Hash s))) = url ++ "?etag=" ++ s
