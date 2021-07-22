{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

module Parser where

import Data.Map (Map)
import Data.Aeson
import Data.Text hiding (singleton, concat)
import Data.Text.Encoding
import Data.Aeson.Lens
import Control.Lens
import Data.HashMap.Strict hiding (toList)
import Control.Monad (join)
import Data.Hashable
import Data.ByteString.Lazy (toStrict, ByteString)
import Data.Maybe
import Data.Vector (toList)
import Text.HTML.TagSoup.Fast
import Text.HTML.TagSoup
import Data.Functor.Foldable
import Text.HTML.Scalpel
import Control.Applicative
import Debug.Trace

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
    join $ scrapeStringLike target =<< extractHtml' <$> decodeStrict (encodeUtf8 guide) <*> pure ""

extractHtml' :: Value -> String -> Scraper Text (Maybe (HashMap Text Text)) 
extractHtml' (String guide) name = do
    result <- html $ tagSelector name
    pure $ Just $ singleton guide result

extractHtml' (Object guide) name = do
    let classes = maybe [] extractClasses $ guide ^. at "class"
    -- let attrs = fromMaybe [] $ extractAttrs <$> guide ^. at "attr"
    let attrs = []
    let getChildren = do
            results <- traverseWithKey (\(keyAndAlias -> (name, alias)) v -> do
                content <- html (TagString (unpack name) @: (classes ++ attrs)) <|> pure ""
                fmap (addAlias alias content) <$> extractHtml' v (unpack name))
                $ guide & at "class" .~ Nothing & at "attr" .~ Nothing
            hmFake <- traverse matchAttrs $ guide ^. at "attr"
            let hm = fromMaybe mempty $ join hmFake
            return $ union hm . flatten <$> sequence results
    if name == ""
        then getChildren
        else chroot (TagString name @: (classes ++ attrs)) getChildren


extractClasses :: Value -> [AttributePredicate]
extractClasses (Object (keys -> guide)) = fmap (hasClass . unpack) guide

extractAttrs :: Value -> [AttributePredicate]
extractAttrs (Object guide) =
    elems $ mapWithKey matchIt guide
    where matchIt k _ = match (\k' _ -> k == pack k')
        --   matchIt k (Object v) = AttributeString (unpack k) @= unpack v

matchAttrs :: Value -> Scraper Text (Maybe (HashMap Text Text))
matchAttrs (Object guide) =
    Just . unions . elems <$> traverseWithKey matchIt guide
    where matchIt k (String v) = singleton v <$> attr (unpack k) anySelector
        --   matchIt k (Object v) = AttributeString (unpack k) @= unpack v
        --   matchIt k Null = match (\k' _ -> k == pack k')

keyAndAlias :: Text -> (Text, Text)
keyAndAlias t = case split (== '=') t of
    [a, b] -> (a, b)
    [a] -> (a, "")

addAlias :: Text -> Text -> HashMap Text Text -> HashMap Text Text
addAlias "" _ m = m
addAlias t val hm = hm & at t ?~ val
