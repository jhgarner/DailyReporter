{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Mock where

import Control.Applicative
import PyF

-- import Theseus.Eff hiding (Eff)
import Theseus.Eff qualified as The (Eff)
import Theseus.Effect.State

newtype Mock effect where
  Mock :: (forall a es ef. effect (The.Eff ef es) a -> The.Eff ef es a) -> Mock effect

data Mocks effect a = Mocks [Mock effect] a
  deriving (Functor)

class InMock effect a where
  (>>) :: [Mock effect] -> [Mock effect] -> a

instance InMock effect [Mock effect] where
  m1 >> m2 = m1 ++ m2

instance InMock effect (Eff (effect : es) a -> Eff es a) where
  m1 >> m2 = interpretMocked $ m1 ++ m2

mock :: (forall a es ef. effect (The.Eff ef es) a -> The.Eff ef es a) -> [Mock effect]
mock f = [Mock f]

mustBe :: (Eq a, Show a) => a -> a -> ()
mustBe a1 a2
  | a1 == a2 = ()
  | otherwise = error [fmt|expected {show a1} but got {show a2}|]

interpretMocked :: forall effect es a. [Mock effect] -> Eff (effect : es) a -> Eff es a
interpretMocked interactions action =
  fmap snd $ with action $ using (runState interactions) $ interpret \sender effect -> do
    Mock nextInteraction <- gets head
    modify @[Mock effect] tail
    pure $ nextInteraction effect

data Terminal environment returnType where
  PrintLine :: String -> Terminal environment ()
  ReadLine :: Terminal environment String

-- makeEffect ''Terminal

runTestTerminal :: Eff (Terminal : es) a -> Eff es a
runTestTerminal = Mock.do
  mock \ReadLine -> pure "Pretend like the user typed this."
  mock \(PrintLine line) -> pure $ line `mustBe` "You entered:"
  mock \(PrintLine line) -> pure $ line `mustBe` "Pretend like the user typed this."
