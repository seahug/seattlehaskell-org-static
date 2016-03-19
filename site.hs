{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import           Data.List (isPrefixOf)
import qualified Data.Map as Map
import           Hakyll
import           System.FilePath.Posix (splitExtension)
import qualified Text.Blaze.Html.Renderer.String as TBHRS
import qualified Text.Hamlet.Runtime as THR

-- Passthrough: do not relativize URLs
fixUpUrls :: Item String -> Compiler (Item String)
fixUpUrls item = do
    itemRoute <- getRoute $ itemIdentifier item
    return $ case itemRoute of
        Nothing -> item
        Just r  -> fmap (fixUpUrlsWith $ toSiteRoot r) item

-- $TODO: Convert to doctests
-- "about.html" -> "about"
-- "about.other" -> "about.other"
normalizePath :: String -> String
normalizePath path =
    let
        (basePath, ext) = splitExtension path
    in
        if ext == ".html"
           then basePath
           else path

-- //about.html -> /about
-- //about.other -> /about.other
renderIntAbsUrl :: String -> String
renderIntAbsUrl = normalizePath . tail

-- http://foo/bar -> http://foo/bar
renderExtAbsUrl :: String -> String
renderExtAbsUrl = id

-- "/about.html" -> "/content/about"
-- "/about.other" -> "/content/about.other"
renderIntRelUrl :: String -> String
renderIntRelUrl = normalizePath . (SITE_ROOT_DIR ++)

-- "#" -> "#"
-- "#anchor" -> "#anchor"
renderAnchorOnly :: String -> String
renderAnchorOnly = id

fixUpUrlsWith :: String -> String -> String
fixUpUrlsWith _ = withUrls fixUpUrl
    where
        fixUpUrl x
            | isIntAbs x            = renderIntAbsUrl x     -- internal absolute URL
            | isExtAbs x            = renderExtAbsUrl x     -- external absolute URL
            | isIntRel x            = renderIntRelUrl x     -- internal relative URL
            | isAnchorOnly x        = renderAnchorOnly x    -- anchor only
            | otherwise             = "[[ERROR:" ++ x ++ "]]"
        isIntAbs = ("//" `isPrefixOf`)
        isExtAbs x = "http://" `isPrefixOf` x || "https://" `isPrefixOf` x
        isIntRel = ("/" `isPrefixOf`)
        isAnchorOnly = ("#" `isPrefixOf`)

-- BEGIN: Miscellaneous filters

renderHamlet :: String -> String
renderHamlet s = do
    template <- THR.parseHamletTemplate THR.defaultHamletSettings s
    html <- THR.renderHamletTemplate template Map.empty
    TBHRS.renderHtml html

-- END: Miscellaneous filters

-- BEGIN: Miscellaneous compilers

htmlPageCompiler :: Identifier -> Context String -> Compiler (Item String)
htmlPageCompiler templatePath ctx =
    getResourceBody
        >>= applyAsTemplate ctx
        >>= loadAndApplyTemplate templatePath ctx
        >>= fixUpUrls

pageCompilerWithFilter :: (String -> String) -> Identifier -> Context String -> Compiler (Item String)
pageCompilerWithFilter f templatePath ctx = do
    body <- getResourceBody
    return $ fmap f body
    >>= applyAsTemplate defaultContext
    >>= loadAndApplyTemplate templatePath ctx
    >>= fixUpUrls

hamletPageCompiler :: Identifier -> Context String -> Compiler (Item String)
hamletPageCompiler = pageCompilerWithFilter renderHamlet

pandocPageCompiler :: Identifier -> Context String -> Compiler (Item String)
pandocPageCompiler templatePath ctx =
    pandocCompiler
          >>= loadAndApplyTemplate templatePath ctx
          >>= fixUpUrls

hamletTemplateCompiler :: Compiler (Item Template)
hamletTemplateCompiler = cached "hamletTemplateCompiler" $ do
    item <- getResourceString
    return $ fmap (readTemplate . renderHamlet) item

externalHamletTemplateCompiler :: Compiler (Item Template)
externalHamletTemplateCompiler = cached "externalHamletTemplateCompiler" $ do
    item <- getResourceString
    item2 <- withItemBody (unixFilter "templategen" []) item
    return $ fmap readTemplate item2

-- END: Miscellaneous compilers

-- BEGIN: Routes

pagesRoute :: Routes
pagesRoute = gsubRoute "pages/" (const "")

-- END: Routes

defaultTemplate :: Identifier
defaultTemplate = "seattlehaskell-org/templates/default-layout-wrapper.hamlet"

main :: IO ()
main = hakyll hakyllMain

hakyllMain :: Rules ()
hakyllMain = do
    match ("images/*" .||. "other/*") $ do
        route   idRoute
        compile copyFileCompiler

    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler

    match "posts/*" $ do
        route $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/post.html"    postCtx
            >>= loadAndApplyTemplate "templates/default.html" postCtx
            >>= fixUpUrls

    create ["archive.html"] $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "posts/*"
            let archiveCtx =
                    listField "posts" postCtx (return posts) `mappend`
                    constField "title" "Archives"            `mappend`
                    defaultContext

            makeItem ""
                >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
                >>= loadAndApplyTemplate "templates/default.html" archiveCtx
                >>= fixUpUrls


    match "index.html" $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "posts/*"
            let indexCtx =
                    listField "posts" postCtx (return posts) `mappend`
                    constField "title" "Home"                `mappend`
                    defaultContext

            htmlPageCompiler defaultTemplate indexCtx

    match "pages/*.hamlet" $ do
        route $ pagesRoute `composeRoutes` setExtension "html"
        compile $ hamletPageCompiler defaultTemplate defaultContext

    match "pages/*.html" $ do
        route pagesRoute
        compile $ htmlPageCompiler defaultTemplate defaultContext

    match "pages/*.md" $ do
        route $ pagesRoute `composeRoutes` setExtension "html"
        compile $ pandocPageCompiler defaultTemplate defaultContext

    match "templates/*.html" $ compile templateCompiler

    match "templates/*.hamlet" $ compile hamletTemplateCompiler

    match (fromGlob $ toFilePath defaultTemplate) $ compile externalHamletTemplateCompiler

--------------------------------------------------------------------------------
postCtx :: Context String
postCtx =
    dateField "date" "%B %e, %Y" `mappend`
    defaultContext
