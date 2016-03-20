module TemplateGen.Css (generateCommonCss) where

import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import           TemplateGen.Templates
import           TemplateGen.Types
import qualified Text.Lucius as L
import qualified Yesod.Static as YS

generateCommonCss :: (TL.Text, Hash)
generateCommonCss =
    let
        text = (L.renderCss . defaultCssTemplate) undefined
        bs = TLE.encodeUtf8 text
        hash = Hash (YS.base64md5 bs)
    in (text, hash)
