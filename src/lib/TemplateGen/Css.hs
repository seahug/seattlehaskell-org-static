module TemplateGen.Css (
    generateDefaultCss
) where

import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import qualified TemplateGen.Hash as H
import qualified TemplateGen.Templates as T
import qualified Text.Lucius as L
import qualified Yesod.Static as YS

generateDefaultCss :: (TL.Text, H.Hash)
generateDefaultCss =
    let
        text = (L.renderCss . T.defaultCssTemplate) undefined
        bs = TLE.encodeUtf8 text
        hash = H.Hash (YS.base64md5 bs)
    in (text, hash)
