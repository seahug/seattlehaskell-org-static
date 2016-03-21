{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module TemplateGen.Html (
    renderHtmlTemplate
  , renderHtmlUrl
) where

import           Data.Text
import qualified TemplateGen.PageContext as PC
import           TemplateGen.Resource
import           TemplateGen.Settings
import qualified TemplateGen.SiteInfo as SI
import qualified TemplateGen.TemplateContext as TC
import qualified TemplateGen.Url as U
import           TemplateGen.Url (Url(AboutR, HomeR)) -- import unqualified for TH use
import           Text.Hamlet

-- A note about URLs as handled by the site generator
-- Internal relative URLs
-- Example: "/about" is converted to "/<root>/about"
-- Internal absolute URLs
-- Example: "//about" is converted to "/about"
-- External absolute URLs
-- Example: "http://foo/bar" is left as is
renderHtmlUrl :: Show a => SI.SiteInfo -> U.Url -> a -> Text
renderHtmlUrl _ U.AboutR _ = "//about"    -- an internal absolute URL
renderHtmlUrl _ U.HomeR _ = "//"          -- an internal absolute URL

#define LAYOUT_TEMPLATE_PATH (TEMPLATE_DIR ++ "/default-layout-wrapper.hamlet")
renderHtmlTemplate :: SI.SiteInfo -> TC.TemplateContext -> HtmlUrl U.Url
renderHtmlTemplate si tc =
    let
        TC.TemplateContext {..} = tc
        pageBody _ = [hamlet|\$body$
$forall r <- SI.scripts si
    <script src=#{fullUrl r}>|]
        pageHead _ = [hamlet|$forall r <- SI.stylesheets si
    <link rel="stylesheet" href=#{fullUrl r}>|]
    in
        $(hamletFile LAYOUT_TEMPLATE_PATH)
