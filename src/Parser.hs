{-# LANGUAGE LambdaCase #-}
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
import Data.Functor.Foldable
import Data.HashMap.Strict hiding (toList)
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

-- JSON

extractJson :: Text -> Text -> Maybe (HashMap Text Text)
extractJson v s = join $ extractJson' <$> decodeStrict (encodeUtf8 v) <*> decodeStrict (encodeUtf8 s)

extractJson' :: Value -> Value -> Maybe (HashMap Text Text)
extractJson' (Object guide) (Object target) =
  flatten <$> traverseWithKey (\(keyAndAlias -> (name, alias)) v -> addAlias alias content <$> (target ^. at name >>= extractJson' v)) guide
  where
    content = toText $ encode $ Object target
extractJson' (Object guide) (Array target) =
  flatten <$> traverseWithKey (\(keyAndAlias -> (name, alias)) v -> addAlias alias content <$> (target ^? ix (read @Int $ unpack name) >>= extractJson' v)) guide
  where
    content = toText $ encode $ Array target
extractJson' (Array (toList -> [String name, String defaultVal])) target =
  Just $ fromMaybe (singleton name defaultVal) $ extractJson' (String name) target
extractJson' (String name) (String target) = Just $ singleton name target
extractJson' (String name) target = Just $ singleton name $ toText $ encode target

flatten :: (Eq x, Hashable x) => HashMap x (HashMap x v) -> HashMap x v
flatten h = unions $ elems h

toText :: ByteString -> Text
toText = decodeUtf8 . toStrict

-- HTML

extractHtml :: Text -> Text -> Maybe (HashMap Text Text)
extractHtml guide target =
  join $ scrapeStringLike target =<< extractHtml' <$> decodeStrict (encodeUtf8 guide)

extractHtml' :: Value -> Scraper Text (Maybe (HashMap Text Text))
extractHtml' (String guide) = do
  result <- html anySelector
  pure $ Just $ singleton guide result
extractHtml' (Object guide) = do
  -- let classes = maybe [] extractClasses $ guide ^. at "class"
  let attrs = maybe [] extractAttrs $ guide ^. at "attr"
  -- content <- html (TagString (unpack name)) <|> pure ""
  results <-
    traverseWithKey
      ( \name v ->
          case splitOn "=" name of
            [name] -> chroot (TagString (unpack name) @: attrs) (extractHtml' v)
            [name, alias] -> do
              rest <- chroot (TagString (unpack name) @: attrs) (extractHtml' v)
              inner <- html $ TagString (unpack name) @: attrs
              pure $ insert alias inner <$> rest
      )
      $ guide & at "attr" .~ Nothing
  hmFake <- traverse matchAttrs $ guide ^. at "attr"
  let hm = fromMaybe mempty $ join hmFake
  return $ union hm . flatten <$> sequence results

extractClasses :: Value -> [AttributePredicate]
extractClasses (Object (keys -> guide)) = fmap (hasClass . unpack) guide

extractAttrs :: Value -> [AttributePredicate]
extractAttrs (Object guide) =
  elems $ mapWithKey matchIt guide
  where
    matchIt k _ = match (\k' _ -> "*" `isPrefixOf` k || k == pack k')

--   matchIt k (Object v) = AttributeString (unpack k) @= unpack v

matchAttrs :: Value -> Scraper Text (Maybe (HashMap Text Text))
matchAttrs (Object guide) =
  Just . unions . elems <$> traverseWithKey matchIt guide
  where
    matchIt k (String v) = singleton v <$> attr (unpack k) anySelector

--   matchIt k (Object v) = AttributeString (unpack k) @= unpack v
--   matchIt k Null = match (\k' _ -> k == pack k')

keyAndAlias :: Text -> (Text, Text)
keyAndAlias t = case split (== '=') t of
  [a, b] -> (a, b)
  [a] -> (a, "")

addAlias :: Text -> Text -> HashMap Text Text -> HashMap Text Text
addAlias "" _ m = m
addAlias t val hm = hm & at t ?~ val
