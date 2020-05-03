-- | Project type
module Project (Id(..), Project(..))
    where

import Relude


-- | The project identifier
newtype Id = Id { unId :: Text }
    deriving (Eq, Ord, Show)


-- | Project data type as described in the sln file
data Project = Project
    { projId :: Id             -- ^ An identifier
    , projName :: Text         -- ^ The name of the project
    , projDependencies :: [Id] -- ^ The list of its dependencies
    }
    deriving Show

