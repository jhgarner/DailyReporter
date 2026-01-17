{-# LANGUAGE AllowAmbiguousTypes #-}

module Tracing where

class Tracable (effect :: Effect) where
  traceEffect :: effect (Eff es) a -> Text

-- runTrace :: forall e es. (Tracable e, _) => Eff es ~> Eff es
-- runTrace = interpose @e \effect -> do
--   liftIO $ putStrLn $ unpack $ traceEffect effect
--   sendVia toEff effect
