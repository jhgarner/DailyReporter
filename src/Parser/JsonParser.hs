{-# LANGUAGE ViewPatterns #-}

module Parser.JsonParser where

import Control.Lens
import Data.Aeson
  ( Key,
    Value (Array, Object, String),
    decodeStrict,
    encode,
  )
import Data.Aeson.Key (fromText, toString, toText)
import Data.Aeson.KeyMap (KeyMap, elems, singleton, toMapText)
import Data.Aeson.Lens ()
import qualified Data.ByteString.Lazy as Lazy (ByteString, toStrict)
import Data.Foldable (fold)
import Data.Map (Map)
import Data.Maybe (fromMaybe)
import Data.Text (Text, split)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Data.Vector as V (toList)
import Prelude hiding (singleton)

-- This file is a little scary. It attempts to define a way to write Json
-- (called a guide) which can pattern match on other Json. It mostly works, but
-- it doesn't have the same semantics (or syntax) as the Html version.

extractJson :: ByteString -> ByteString -> Maybe (Map Text Text)
extractJson v s = do
  v' <- decodeStrict v
  s' <- decodeStrict s
  keyMap <- extractJson' v' s'
  pure $ toMapText keyMap

extractJson' :: Value -> Value -> Maybe (KeyMap Text)
extractJson' (Object guide) (Object target) =
  flatten <$> itraverse (\(keyAndAlias -> (name, alias)) v -> addAlias alias content <$> (target ^. at name >>= extractJson' v)) guide
  where
    content = byteStringToText $ encode $ Object target
extractJson' (Object guide) (Array target) =
  flatten <$> itraverse (\(keyAndAlias -> (name, alias)) v -> addAlias alias content <$> (target ^? ix (read @Int $ toString name) >>= extractJson' v)) guide
  where
    content = byteStringToText $ encode $ Array target
extractJson' (Array (V.toList -> [String name, String defaultVal])) target =
  Just $ fromMaybe (singleton (fromText name) defaultVal) $ extractJson' (String name) target
extractJson' (String name) (String target) = Just $ singleton (fromText name) target
extractJson' (String name) target = Just $ singleton (fromText name) $ byteStringToText $ encode target

flatten :: KeyMap (KeyMap v) -> KeyMap v
flatten h = fold $ elems h

byteStringToText :: Lazy.ByteString -> Text
byteStringToText = decodeUtf8 . Lazy.toStrict

keyAndAlias :: Key -> (Key, Key)
keyAndAlias t = case split (== '=') (toText t) of
  [a, b] -> (fromText a, fromText b)
  [a] -> (fromText a, "")

addAlias :: Key -> Text -> KeyMap Text -> KeyMap Text
addAlias "" _ m = m
addAlias t val hm = hm & at t ?~ val
