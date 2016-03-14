{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

module TemplateGen.DoIt (
    doIt
) where

import qualified Data.ByteString.Lazy as BSL
import           TemplateGen.Css
import           TemplateGen.Html
import           TemplateGen.Resource
import           TemplateGen.SiteInfo
import           TemplateGen.Types
import           Text.Blaze.Html.Renderer.String (renderHtml)
import           Yesod.Static

readResources :: FilePath -> [FilePath] -> IO [Resource]
readResources dir = mapM $ \x ->
    let relativePath = dir ++ "/" ++ x
    in readResource (staticFilePath relativePath) (staticUrl relativePath)
    where
        readResource :: FilePath -> UrlString -> IO Resource
        readResource path url = do
            bs <- BSL.readFile path
            return $ Resource url (Just $ Hash (base64md5 bs))
        staticFilePath :: FilePath -> FilePath
        staticFilePath path = "seattlehaskell-org/static/" ++ path
        staticUrl :: FilePath -> UrlString
        staticUrl path = "//static/" ++ path -- internal absolute URLs

-- Configuration: should store this information externally
localStylesheetFileNames :: [FilePath]
localStylesheetFileNames = ["bootstrap.css", "haskell.font.css"]

externalScriptUrls :: [UrlString]
externalScriptUrls = ["https://ajax.googleapis.com/ajax/libs/jquery/1.11.3/jquery.min.js"]

localScriptFileNames :: [FilePath]
localScriptFileNames = ["bootstrap.min.js", "ie10-viewport-bug-workaround.js"]
-- End of configuration

-- Internal absolute URL
getAutogenCssResource :: IO Resource
getAutogenCssResource = do
    let (_, hash) = generateCommonCss
    return $ Resource ("//static/tmp/autogen-" ++ toString hash ++ ".css") Nothing

generateHtmlTemplateFile :: SiteInfo -> IO ()
generateHtmlTemplateFile si = do
    let html = renderHtmlTemplate si defaultTemplateContext (renderHtmlUrl si)
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
