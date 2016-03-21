module TemplateGen.Settings (
    Settings(..)
) where

data Settings = Settings {
    copyrightYear :: String,
    copyright :: String,
    analytics :: Maybe String
}
