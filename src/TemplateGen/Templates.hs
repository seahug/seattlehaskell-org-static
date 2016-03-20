{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}

module TemplateGen.Templates (
    defaultCssTemplate
) where

import qualified Text.Lucius as L

#define CSS_TEMPLATE_PATH (TEMPLATE_DIR ++ "/default-layout.lucius")
defaultCssTemplate :: L.CssUrl ()
#if RELEASE
defaultCssTemplate = $(L.luciusFile CSS_TEMPLATE_PATH)
#else
defaultCssTemplate = $(L.luciusFileDebug CSS_TEMPLATE_PATH)
#endif
