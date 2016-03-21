module TemplateGen.TemplateContext (
    TemplateContext(..)
  , mkTemplateContext
) where

import qualified TemplateGen.Master as M
import qualified TemplateGen.PageContext as PC
import qualified TemplateGen.Settings as S
import qualified TemplateGen.Url as U

data TemplateContext = TemplateContext {
    pageTitle :: PC.PageContext -> String,
    pc :: PC.PageContext,
    currentRoute :: Maybe U.Url,
    appSettings :: M.Master -> S.Settings,
    appCopyrightYear :: S.Settings -> String,
    appCopyright :: S.Settings -> String,
    appAnalytics :: S.Settings -> Maybe String,
    master :: M.Master
}

mkTemplateContext :: S.Settings -> PC.PageContext -> TemplateContext
mkTemplateContext s pc = TemplateContext
    PC.title -- pageTitle
    pc
    Nothing -- currentRoute
    (\(M.Master s) -> s) -- appSettings
    S.copyrightYear -- appCopyrightYear
    S.copyright -- appCopyright
    S.analytics -- appAnalytics
    (M.Master s) -- master
