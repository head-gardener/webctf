{-# OPTIONS_GHC -Wno-orphans #-}

module Application where

import Foundation
import Yesod
import Routers
import Yesod.Auth (getAuth)

mkYesodDispatch "WebCTF" resourcesWebCTF
