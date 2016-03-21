module TemplateGen.PageContext (
    PageContext(..)
  , mkPageContext
) where

data PageContext = PageContext {
    title :: String
}

mkPageContext :: PageContext
mkPageContext = PageContext ""
