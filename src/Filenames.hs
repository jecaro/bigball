module Filenames
    where

import Relude

import Control.Monad.Catch
import Path


allGraph :: Text
allGraph = "all"


withJsExt :: MonadThrow m => Path a File -> m (Path a File)
withJsExt = addExtension ".js"


stringToJs :: (MonadThrow m) => Text -> m (Path Rel File)
stringToJs name = parseRelFile (toString name) >>= withJsExt


allGraphJs :: MonadThrow m => m (Path Rel File)
allGraphJs = stringToJs allGraph


directGraph :: Text -> Text
directGraph name = name <> "_direct"


directGraphJs :: MonadThrow m => Text -> m (Path Rel File)
directGraphJs name = stringToJs $ directGraph name 


indirectGraph :: Text -> Text
indirectGraph name = name <> "_indirect"


indirectGraphJs :: MonadThrow m => Text -> m (Path Rel File)
indirectGraphJs name = stringToJs $ indirectGraph name
