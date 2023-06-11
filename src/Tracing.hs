module Tracing where

class Tracable (effect :: Effect) where
  traceEffect :: effect (Eff es) a -> Text
