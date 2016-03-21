module TemplateGen.PageContext (
    PageContext
  , mkPageContext
  , title
) where

data PageContext = PageContext {
    title :: String
}

mkPageContext :: PageContext
mkPageContext = PageContext ""
