{-# LANGUAGE CPP #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Templates (
    defaultCssTemplate
  , renderHtmlTemplate
) where

import qualified TemplateGen.Resource as R
import qualified TemplateGen.SiteInfo as SI
import qualified TemplateGen.TemplateContext as TC
import qualified TemplateGen.Url as U
import           TemplateGen.Url (Url(AboutR, HomeR)) -- unqualified import required by Template Haskell
import qualified Text.Hamlet as H
import qualified Text.Lucius as L

#define CSS_TEMPLATE_PATH (TEMPLATE_DIR ++ "/default-layout.lucius")
defaultCssTemplate :: L.CssUrl ()
#if RELEASE
defaultCssTemplate = $(L.luciusFile CSS_TEMPLATE_PATH)
#else
defaultCssTemplate = $(L.luciusFileDebug CSS_TEMPLATE_PATH)
#endif

#define LAYOUT_TEMPLATE_PATH (TEMPLATE_DIR ++ "/default-layout-wrapper.hamlet")
renderHtmlTemplate :: SI.SiteInfo -> TC.TemplateContext -> H.HtmlUrl U.Url
renderHtmlTemplate si tc =
    let
        TC.TemplateContext {..} = tc
        pageBody _ = [H.hamlet|\$body$
$forall r <- SI.scripts si
    <script src=#{R.fullUrl r}>|]
        pageHead _ = [H.hamlet|$forall r <- SI.stylesheets si
    <link rel="stylesheet" href=#{R.fullUrl r}>|]
    in
        $(H.hamletFile LAYOUT_TEMPLATE_PATH)
