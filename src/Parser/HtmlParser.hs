{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Parser.HtmlParser (extractHtml) where

import Data.Aeson
  ( FromJSON (parseJSON),
    withObject,
    (.:), eitherDecodeStrict, (.:?), (.!=),
  )
import Data.Text (Text, isPrefixOf, unpack)
import Data.Vector (toList)
import Text.HTML.Scalpel
import Text.HTML.TagSoup.Fast (parseTagsT)

-- Defines a form of pattern matching on html

-- Enables using Scraper with fold in a less verbose way
deriving via Ap (Scraper text) a instance Semigroup a => Semigroup (Scraper text a)

deriving via Ap (Scraper text) a instance Monoid a => Monoid (Scraper text a)

data Tagged = Tagged
  { tagName :: String,
    tagAlias :: Maybe Text,
    tagAttrs :: Map Text Text,
    tagInner :: [Tagged]
  }
  deriving (Show)
makeBaseFunctor ''Tagged

instance FromJSON Tagged where
  parseJSON = withObject "Tags" parseItem
    where
      parseItem v =
        Tagged 
          <$> v .: "name"
          <*> v .:? "alias"
          <*> v .:? "attr" .!= mempty
          <*> v .:? "inner" .!= mempty

extractHtml :: ByteString -> ByteString -> Map Text Text
extractHtml encodedGuide target =
  fromMaybe (error "invalid html") $
    scrape scraper (parseTagsT target)
  where
    guide :: [Tagged]
    guide = either error id $ eitherDecodeStrict encodedGuide
    scraper = foldMap (cata extractHtml') guide

type GuideScraper = Scraper Text (Map Text Text)

extractHtml' :: TaggedF GuideScraper -> GuideScraper
extractHtml' TaggedF {..} = do
  let selector = TagString tagNameF @: extractAttrs tagAttrsF
  withNested <- chroot selector (fold tagInnerF)
  withAlias <- foldMap (extractAlias selector) tagAliasF
  withOwnAttrs <- matchAttrs selector tagAttrsF
  return $ fold [withNested, withAlias, withOwnAttrs]

extractAlias :: Selector -> Text -> GuideScraper
extractAlias selector alias = singleton alias <$> html selector

extractAttrs :: Map Text Text -> [AttributePredicate]
extractAttrs =
  ifoldMap' matchIt
  where
    matchIt k v
      | "*" `isPrefixOf` v = pure $ match (\k' _ -> unpack k == k')
      | otherwise = pure $ AttributeString (unpack k) @= unpack v

matchAttrs :: Selector -> Map Text Text -> Scraper Text (Map Text Text)
matchAttrs selector tagAttrs = do
  ifoldMap' matchIt tagAttrs
  where
    matchIt k v
      | "*" `isPrefixOf` v = singleton v <$> attr (unpack k) selector
      | otherwise = mempty