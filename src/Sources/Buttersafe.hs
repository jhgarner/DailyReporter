
module Sources.Buttersafe (butter) where

import Sources.Lib

butter :: _ => Source es
butter = makeSource "Buttersafe" $ usingHtmlUrl $ https"buttersafe.com"
