module TemplateGen.Resource (
    Resource(..)
  , fullUrl
) where

import TemplateGen.Types

data Resource = Resource UrlString (Maybe Hash) deriving Show

fullUrl :: Resource -> UrlString
fullUrl (Resource url Nothing) = url
fullUrl (Resource url (Just (Hash s))) = url ++ "?etag=" ++ s
