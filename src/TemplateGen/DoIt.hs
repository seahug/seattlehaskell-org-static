{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

module TemplateGen.DoIt (
    doIt
) where

import qualified Data.ByteString.Lazy as BSL
import qualified TemplateGen.Css as C
import qualified TemplateGen.Hash as H
import           TemplateGen.Html
import qualified TemplateGen.PageContext as PC
import           TemplateGen.Resource
import qualified TemplateGen.Settings as S
import           TemplateGen.SiteInfo
import qualified TemplateGen.TemplateContext as TC
import qualified TemplateGen.UrlString as US
import           Text.Blaze.Html.Renderer.String (renderHtml)
import           Yesod.Static

readResources :: FilePath -> [FilePath] -> IO [Resource]
readResources dir = mapM $ \x ->
    let relativePath = dir ++ "/" ++ x
    in readResource (staticFilePath relativePath) (staticUrl relativePath)
    where
        readResource :: FilePath -> US.UrlString -> IO Resource
        readResource path url = do
            bs <- BSL.readFile path
            return $ Resource url (Just $ H.Hash (base64md5 bs))
        staticFilePath :: FilePath -> FilePath
        staticFilePath path = "seattlehaskell-org/static/" ++ path
        staticUrl :: FilePath -> US.UrlString
        staticUrl path = "//static/" ++ path -- internal absolute URLs

-- Configuration: should store this information externally
localStylesheetFileNames :: [FilePath]
localStylesheetFileNames = ["bootstrap.css", "haskell.font.css"]

externalScriptUrls :: [US.UrlString]
externalScriptUrls = ["https://ajax.googleapis.com/ajax/libs/jquery/1.11.3/jquery.min.js"]

localScriptFileNames :: [FilePath]
localScriptFileNames = ["bootstrap.min.js", "ie10-viewport-bug-workaround.js"]
-- End of configuration

-- Internal absolute URL
getAutogenCssResource :: IO Resource
getAutogenCssResource = do
    let (_, hash) = C.generateDefaultCss
    return $ Resource ("//static/tmp/autogen-" ++ H.toString hash ++ ".css") Nothing

generateHtmlTemplateFile :: SiteInfo -> IO ()
generateHtmlTemplateFile si = do
    let
        settings = S.Settings "2016" "Seattle Area Haskell Users' Group" Nothing
        pageCtx = PC.mkPageContext { PC.title = "SeaHUG - $title$" }
        templateCtx = TC.mkTemplateContext settings pageCtx
        html = renderHtmlTemplate si templateCtx (renderHtmlUrl si)
    putStrLn (renderHtml html)

doIt :: IO ()
doIt = do
    localStylesheets <- readResources "css" localStylesheetFileNames
    let externalScripts = map (`Resource` Nothing) externalScriptUrls
    localScripts <- readResources "js" localScriptFileNames
    let scripts = externalScripts ++ localScripts

    commonCssResource <- getAutogenCssResource

    let
        stylesheets = localStylesheets ++ [commonCssResource]
        siteInfo = SiteInfo stylesheets scripts
    generateHtmlTemplateFile siteInfo
