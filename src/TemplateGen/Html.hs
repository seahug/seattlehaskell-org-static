{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module TemplateGen.Html (
    defaultTemplateContext
  , renderHtmlTemplate
  , renderHtmlUrl
) where

import Data.Text
import qualified TemplateGen.PageContext as PC
import TemplateGen.Resource
import TemplateGen.SiteInfo
import TemplateGen.Types
import Text.Hamlet

data Settings = Settings {
    copyrightYear :: String,
    copyright :: String,
    analytics :: Maybe String
}

defaultSettings :: Settings
defaultSettings = Settings "2016" "Seattle Area Haskell Users' Group" Nothing

data Master = Master Settings

data TemplateContext = TemplateContext {
    pageTitle :: PC.PageContext -> String,
    pc :: PC.PageContext,
    currentRoute :: Maybe Url,
    appSettings :: Master -> Settings,
    appCopyrightYear :: Settings -> String,
    appCopyright :: Settings -> String,
    appAnalytics :: Settings -> Maybe String,
    master :: Master
}

defaultTemplateContext :: TemplateContext
defaultTemplateContext = TemplateContext
    PC.title -- pageTitle
    PC.mkDefault { PC.title = "SeaHUG - $title$" } -- pc
    Nothing -- currentRoute
    (\(Master s) -> s) -- appSettings
    copyrightYear -- appCopyrightYear
    copyright -- appCopyright
    analytics -- appAnalytics
    (Master defaultSettings) -- master

-- A note about URLs as handled by the site generator
-- Internal relative URLs
-- Example: "/about" is converted to "/<root>/about"
-- Internal absolute URLs
-- Example: "//about" is converted to "/about"
-- External absolute URLs
-- Example: "http://foo/bar" is left as is
renderHtmlUrl :: Show a => SiteInfo -> Url -> a -> Text
renderHtmlUrl _ AboutR _ = "//about"    -- an internal absolute URL
renderHtmlUrl _ HomeR _ = "//"          -- an internal absolute URL

#define LAYOUT_TEMPLATE_PATH (TEMPLATE_DIR ++ "/default-layout-wrapper.hamlet")
renderHtmlTemplate :: SiteInfo -> TemplateContext -> HtmlUrl Url
renderHtmlTemplate si tc =
    let
        SiteInfo {..} = si
        TemplateContext {..} = tc
        pageBody _ = [hamlet|\$body$
$forall r <- siteInfoScripts
    <script src=#{fullUrl r}>|]
        pageHead _ = [hamlet|$forall r <- siteInfoStylesheets
    <link rel="stylesheet" href=#{fullUrl r}>|]
    in
        $(hamletFile LAYOUT_TEMPLATE_PATH)
