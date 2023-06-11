module Sources.Ec (ec) where

import Sources.Lib

ec :: _ => Source es
ec = makeSource "Ec" $ usingHtmlUrl $ https"existentialcomics.com"
