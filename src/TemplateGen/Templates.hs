{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}

module TemplateGen.Templates (
    commonCssTemplate
) where

import qualified Text.Lucius as L

#define CSS_TEMPLATE_PATH (TEMPLATE_DIR ++ "/default-layout.lucius")
commonCssTemplate :: L.CssUrl ()
#if PRODUCTION
commonCssTemplate = $(L.luciusFile CSS_TEMPLATE_PATH)
#else
commonCssTemplate = $(L.luciusFileDebug CSS_TEMPLATE_PATH)
#endif
