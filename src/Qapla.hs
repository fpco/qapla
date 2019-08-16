{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Qapla
  ( qapla
  ) where

import RIO
import qualified RIO.ByteString as B
import qualified RIO.Text as T
import RIO.FilePath
import RIO.List (stripPrefix, isSuffixOf, isPrefixOf)
import qualified RIO.Map as Map
import Conduit
import Network.Wai
import Network.HTTP.Types
import Network.Mime
import qualified Text.HTML.DOM as H
import Text.XML.Cursor (fromDocument, ($//), element, attributeIs, attribute)

data Resource
  = ResourceRedirect !ByteString
  | ResourceContent !Content
  deriving Show

data Content = Content
  { contentMime :: !ByteString
  , contentFile :: !FilePath
  }
  deriving Show

data Contents = Contents
  { contentsNotFound :: !(Maybe FilePath)
  , contentsMap :: !(Map ByteString Resource)
  }

-- | Create a WAI application for hosting the generated files in the given directory.
qapla :: MonadIO m => FilePath -> m Application
qapla root = liftIO $ do
  Contents mnf m <-
    runConduitRes $
    sourceDirectoryDeep False root .|
    filterC (not . ignored) .|
    foldMC (add (addTrailingPathSeparator root)) (Contents Nothing mempty)
  nf <-
    case mnf of
      Nothing -> throwIO No404File
      Just nf -> pure nf
  pure $ \req send -> do
    let raw = fromMaybe (assert False (rawPathInfo req)) (B.stripPrefix "/" (rawPathInfo req))
    send $
      case B.stripSuffix "/" raw of
        Just noTrail -> responseBuilder status303 [("location", "/" <> noTrail)] mempty
        Nothing ->
          case Map.lookup raw m of
            Nothing -> responseFile status404 [("content-type", html)] nf Nothing
            Just (ResourceRedirect dest) ->
              responseBuilder status303 [("location", dest)] mempty
            Just (ResourceContent (Content mime file)) ->
              responseFile status200 [("content-type", mime)] file Nothing

ignored :: FilePath -> Bool
ignored fp =
  "~" `isSuffixOf` fp ||
  "." `isPrefixOf` (takeFileName fp)

add
  :: MonadIO m
  => FilePath -- ^ root dir
  -> Contents
  -> FilePath -- ^ new file
  -> m Contents
add root c new = do
  rel <-
    case stripPrefix root new of
      Nothing -> throwIO $ StripPrefixFailed root new
      Just rel -> pure $ encodeUtf8 $ fromString rel
  let add' path content =
        case Map.lookup path $ contentsMap c of
          Nothing -> pure c { contentsMap = Map.insert path content $ contentsMap c }
          Just orig -> throwIO $ MultipleEntries path [orig, content]
  case rel of
    "rss.xml" -> add' "rss.xml" $ ResourceContent Content
      { contentMime = "application/rss+xml"
      , contentFile = new
      }
    "404.html" ->
      case contentsNotFound c of
        Just orig -> throwIO $ Multiple404Files [orig, new]
        Nothing -> pure c { contentsNotFound = Just new }
    _ | Just newRel <- B.stripSuffix "/index.html" rel <|> (guard (rel == "index.html") $> "") -> do
      mredirect <- checkRedirect new
      add' newRel $
        case mredirect of
          Nothing -> ResourceContent Content
            { contentMime = html
            , contentFile = new
            }
          Just red -> ResourceRedirect red
    _ -> add' rel $ ResourceContent Content
            { contentMime = defaultMimeLookup $ fromString new
            , contentFile = new
            }

html :: ByteString
html = "text/html; charset=utf-8"

checkRedirect :: MonadIO m => FilePath -> m (Maybe ByteString)
checkRedirect fp = liftIO $ do
  doc <- H.readFile fp
  case fromDocument doc $// element "meta" >=> attributeIs "http-equiv" "refresh" >=> attribute "content" of
    [] -> pure Nothing
    [x] ->
      case T.stripPrefix "0;url=" x of
        Nothing -> throwIO $ UnhandledRefreshTag x
        Just y -> pure $ Just $ encodeUtf8 y
    xs -> throwIO $ MultipleRefreshTags fp xs

data QaplaException
  = No404File
  | Multiple404Files ![FilePath]
  | StripPrefixFailed !FilePath !FilePath
  | MultipleEntries !ByteString ![Resource]
  | MultipleRefreshTags !FilePath ![Text]
  | UnhandledRefreshTag !Text
  deriving (Show, Typeable)
instance Exception QaplaException
