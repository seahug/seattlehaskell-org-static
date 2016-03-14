module TemplateGen.Types (
    Hash(Hash)
  , Url(AboutR, HomeR)
  , UrlString
  , toString
) where

newtype Hash = Hash String deriving Show

toString :: Hash -> String
toString (Hash s) = s

data Url = AboutR | HomeR deriving Eq

type UrlString = String
