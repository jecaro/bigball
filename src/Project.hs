module Project (Id(..), Project(..))
    where

import Relude


newtype Id = Id { unId :: Text }
    deriving (Eq, Ord, Show)


data Project = Project
    { projId :: Id
    , projName :: Text
    , projDependencies :: [Id]
    }
    deriving Show

