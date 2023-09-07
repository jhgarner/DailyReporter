{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QualifiedDo #-}

module Mock where
import Cleff
import PyF
import Cleff.State
import Control.Applicative

newtype Mock effect = Mock (forall a es. effect (Eff es) a -> Eff es a)

data Mocks effect a = Mocks [Mock effect] a
    deriving (Functor)

class InMock effect a where
    (>>) :: [Mock effect] -> [Mock effect] -> a

instance InMock effect [Mock effect] where
    m1 >> m2 = m1 ++ m2

instance InMock effect (Eff (effect:es) a -> Eff es a) where
    m1 >> m2 = interpretMocked $ m1 ++ m2

mock :: (forall a es. effect (Eff es) a -> Eff es a) -> [Mock effect]
mock f = [Mock f]

mustBe :: (Eq a, Show a) => a -> a -> ()
mustBe a1 a2
  | a1 == a2 = ()
  | otherwise = error [fmt|expected {show a1} but got {show a2}|]

interpretMocked :: forall effect es a. [Mock effect] -> Eff (effect:es) a -> Eff es a
interpretMocked interactions = fmap fst . runState interactions . reinterpret \effect -> do
    Mock nextInteraction <- gets head
    modify @[Mock effect] tail
    toEff $ nextInteraction effect

data Terminal environment returnType where
    PrintLine :: String -> Terminal environment ()
    ReadLine :: Terminal environment String
makeEffect ''Terminal

runTestTerminal :: Eff (Terminal:es) a -> Eff es a
runTestTerminal = Mock.do
    mock \ReadLine -> "Pretend like the user typed this."
    mock \(PrintLine line) -> pure $ line `mustBe` "You entered:"
    mock \(PrintLine line) -> pure $ line `mustBe` "Pretend like the user typed this."
