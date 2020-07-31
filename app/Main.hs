{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Lib
import Aws.Lambda

handler :: () -> Context -> IO (Either String ())
handler person context = fmap Right mailReport

-- Use this action if you want to have context that is shared between lambda calls.
-- It is called once per every cold start. We do not wish to have a shared context for our lambda, so we simply use Unit.
initializeContext :: IO ()
initializeContext = return ()

generateLambdaDispatcher StandaloneLambda defaultDispatcherOptions

-- main :: IO ()
-- main = printEmailHtml >>= print
