-- | Some constants and static filenames
module Filenames (allGraph, allGraphJs, level1GraphJs, fullGraphJs) where

import Control.Monad.Catch (MonadThrow)
import Path (File, Path, Rel, addExtension, parseRelFile)
import Relude

allGraph :: Text
allGraph = "all"

withJsExt :: MonadThrow m => Path a File -> m (Path a File)
withJsExt = addExtension ".js"

stringToJs :: (MonadThrow m) => Text -> m (Path Rel File)
stringToJs name = parseRelFile (toString name) >>= withJsExt

allGraphJs :: MonadThrow m => m (Path Rel File)
allGraphJs = stringToJs allGraph

level1Graph :: Text -> Text
level1Graph name = name <> "_level1"

level1GraphJs :: MonadThrow m => Text -> m (Path Rel File)
level1GraphJs name = stringToJs $ level1Graph name

fullGraph :: Text -> Text
fullGraph name = name <> "_full"

fullGraphJs :: MonadThrow m => Text -> m (Path Rel File)
fullGraphJs name = stringToJs $ fullGraph name
