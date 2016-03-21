{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as DT
import qualified TemplateGen.Css as C
import qualified TemplateGen.Hash as H
import qualified TemplateGen.PageContext as PC
import qualified TemplateGen.Resource as R
import qualified TemplateGen.Settings as S
import qualified TemplateGen.SiteInfo as SI
import qualified TemplateGen.TemplateContext as TC
import qualified TemplateGen.Templates as T
import qualified TemplateGen.Url as U
import qualified TemplateGen.UrlString as US
import           Text.Blaze.Html.Renderer.String (renderHtml)
import           Yesod.Static

-- A note about URLs as handled by the site generator
-- Internal relative URLs
-- Example: "/about" is converted to "/<root>/about"
-- Internal absolute URLs
-- Example: "//about" is converted to "/about"
-- External absolute URLs
-- Example: "http://foo/bar" is left as is
renderHtmlUrl :: Show a => SI.SiteInfo -> U.Url -> a -> DT.Text
renderHtmlUrl _ U.AboutR _ = "//about"    -- an internal absolute URL
renderHtmlUrl _ U.HomeR _ = "//"          -- an internal absolute URL

readResources :: FilePath -> [FilePath] -> IO [R.Resource]
readResources dir = mapM $ \x ->
    let relativePath = dir ++ "/" ++ x
    in readResource (staticFilePath relativePath) (staticUrl relativePath)
    where
        readResource :: FilePath -> US.UrlString -> IO R.Resource
        readResource path url = do
            bs <- BSL.readFile path
            return $ R.Resource url (Just $ H.Hash (base64md5 bs))
        staticFilePath :: FilePath -> FilePath
        staticFilePath path = "seattlehaskell-org/static/" ++ path
        staticUrl :: FilePath -> US.UrlString
        staticUrl path = "//static/" ++ path -- internal absolute URLs

-- Configuration: should store this information externally
localStylesheetFileNames :: [FilePath]
localStylesheetFileNames = ["bootstrap.css", "haskell.font.css"]

externalScriptUrls = ["https://ajax.googleapis.com/ajax/libs/jquery/1.11.3/jquery.min.js"]
externalScriptUrls :: [US.UrlString]

localScriptFileNames :: [FilePath]
localScriptFileNames = ["bootstrap.min.js", "ie10-viewport-bug-workaround.js"]
-- End of configuration

-- Internal absolute URL
mkDefaultCssResource :: IO R.Resource
mkDefaultCssResource = do
    let (_, hash) = C.generateDefaultCss
    return $ R.Resource ("//static/tmp/autogen-" ++ H.toString hash ++ ".css") Nothing

generateHtmlTemplateFile :: SI.SiteInfo -> IO ()
generateHtmlTemplateFile si = do
    let
        settings = S.Settings "2016" "Seattle Area Haskell Users' Group" Nothing
        pageCtx = PC.mkPageContext { PC.title = "SeaHUG - $title$" }
        templateCtx = TC.mkTemplateContext settings pageCtx
        html = T.renderHtmlTemplate si templateCtx (renderHtmlUrl si)
    putStrLn (renderHtml html)

main :: IO ()
main = do
    localStylesheets <- readResources "css" localStylesheetFileNames
    let externalScripts = map (`R.Resource` Nothing) externalScriptUrls
    localScripts <- readResources "js" localScriptFileNames
    let scripts = externalScripts ++ localScripts

    defaultStylesheet <- mkDefaultCssResource

    let
        stylesheets = localStylesheets ++ [defaultStylesheet]
        siteInfo = SI.SiteInfo stylesheets scripts
    generateHtmlTemplateFile siteInfo
