module TemplateGen.Hash (
    Hash(..)
  , toString
) where

newtype Hash = Hash String deriving Show

toString :: Hash -> String
toString (Hash s) = s
