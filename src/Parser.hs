{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

module Parser where

import Control.Applicative
import Control.Lens
import Control.Monad (join)
import Data.Aeson
import Data.Aeson.Lens
import Data.ByteString.Lazy (ByteString, toStrict)
-- import Data.Functor.Foldable
-- import Data.HashMap.Strict hiding (toList)
import Data.Hashable
import Data.Map (Map)
import Data.Maybe
import Data.Text hiding (concat, singleton)
import Data.Text.Encoding
import Data.Vector (toList)
import Debug.Trace
import Text.HTML.Scalpel
import Text.HTML.TagSoup
import Text.HTML.TagSoup.Fast
import Data.Aeson.Key (fromString, fromText, toText, toString)
import Data.Aeson.KeyMap (KeyMap, elems, union, singleton, insert, keys, toHashMapText)
import Data.Foldable (fold)
import Data.Functor.WithIndex
import Data.HashMap.Internal.Strict (HashMap)

-- JSON

extractJson :: Text -> Text -> Maybe (HashMap Text Text)
extractJson v s = do
  v' <- decodeStrict (encodeUtf8 v)
  s' <- decodeStrict (encodeUtf8 s)
  keyMap <- extractJson' v' s'
  pure $ toHashMapText keyMap
  

extractJson' :: Value -> Value -> Maybe (KeyMap Text)
extractJson' (Object guide) (Object target) =
  flatten <$> itraverse (\(keyAndAlias -> (name, alias)) v -> addAlias alias content <$> (target ^. at name >>= extractJson' v)) guide
  where
    content = byteStringToText $ encode $ Object target
extractJson' (Object guide) (Array target) =
  flatten <$> itraverse (\(keyAndAlias -> (name, alias)) v -> addAlias alias content <$> (target ^? ix (read @Int $ toString name) >>= extractJson' v)) guide
  where
    content = byteStringToText $ encode $ Array target
extractJson' (Array (toList -> [String name, String defaultVal])) target =
  Just $ fromMaybe (singleton (fromText name) defaultVal) $ extractJson' (String name) target
extractJson' (String name) (String target) = Just $ singleton (fromText name) target
extractJson' (String name) target = Just $ singleton (fromText name) $ byteStringToText $ encode target

flatten :: KeyMap (KeyMap v) -> KeyMap v
flatten h = fold $ elems h

byteStringToText :: ByteString -> Text
byteStringToText = decodeUtf8 . toStrict

-- HTML

extractHtml :: Text -> Text -> Maybe (HashMap Text Text)
extractHtml guide target =
  toHashMapText <$> join (scrapeStringLike target . extractHtml' =<< decodeStrict (encodeUtf8 guide))

extractHtml' :: Value -> Scraper Text (Maybe (KeyMap Text))
extractHtml' (String guide) = do
  result <- html anySelector
  pure $ Just $ singleton (fromText guide) result
extractHtml' (Object guide) = do
  -- let classes = maybe [] extractClasses $ guide ^. at "class"
  let attrs = maybe [] extractAttrs $ guide ^. at "attr"
  -- content <- html (TagString (unpack name)) <|> pure ""
  results <-
    itraverse
      ( \name v ->
          case splitOn "=" (toText name) of
            [name] -> chroot (TagString (unpack name) @: attrs) (extractHtml' v)
            [name, alias] -> do
              rest <- chroot (TagString (unpack name) @: attrs) (extractHtml' v)
              inner <- html $ TagString (unpack name) @: attrs
              pure $ insert (fromText alias) inner <$> rest
      )
      $ guide & at "attr" .~ Nothing
  hmFake <- traverse matchAttrs $ guide ^. at "attr"
  let hm = fromMaybe mempty $ join hmFake
  return $ union hm . flatten <$> sequence results

extractClasses :: Value -> [AttributePredicate]
extractClasses (Object (keys -> guide)) = fmap (hasClass . toString) guide

extractAttrs :: Value -> [AttributePredicate]
extractAttrs (Object guide) =
  elems $ imap matchIt guide
  where
    matchIt k _ = match (\k' _ -> "*" `isPrefixOf` toText k || k == fromString k')

--   matchIt k (Object v) = AttributeString (unpack k) @= unpack v

matchAttrs :: Value -> Scraper Text (Maybe (KeyMap Text))
matchAttrs (Object guide) =
  Just . fold . elems <$> itraverse matchIt guide
  where
    matchIt k (String v) = singleton (fromText v) <$> attr (toString k) anySelector

--   matchIt k (Object v) = AttributeString (unpack k) @= unpack v
--   matchIt k Null = match (\k' _ -> k == pack k')

keyAndAlias :: Key -> (Key, Key)
keyAndAlias t = case split (== '=') (toText t) of
  [a, b] -> (fromText a, fromText b)
  [a] -> (fromText a, "")

addAlias :: Key -> Text -> KeyMap Text -> KeyMap Text
addAlias "" _ m = m
addAlias t val hm = hm & at t ?~ val
