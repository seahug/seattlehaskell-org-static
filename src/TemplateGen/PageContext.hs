module TemplateGen.PageContext (
    PageContext
  , mkDefault
  , title
) where

data PageContext = PageContext {
    title :: String
}

mkDefault :: PageContext
mkDefault = PageContext ""
