module TemplateGen.SiteInfo (
    SiteInfo(..)
) where

import qualified TemplateGen.Resource as R

data SiteInfo = SiteInfo {
    stylesheets :: [R.Resource],
    scripts :: [R.Resource]
}
