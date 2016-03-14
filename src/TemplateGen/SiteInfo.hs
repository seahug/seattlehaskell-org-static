module TemplateGen.SiteInfo (
    SiteInfo(..)
) where

import TemplateGen.Resource

data SiteInfo = SiteInfo {
    siteInfoStylesheets :: [Resource],
    siteInfoScripts :: [Resource]
}
