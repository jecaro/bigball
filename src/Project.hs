-- | Project type
module Project (Id (..), Project (..)) where

import Relude

-- | The project identifier
newtype Id = Id {unId :: Text}
  deriving (Eq, Ord, Show)

-- | Project data type as described in the sln file
data Project = Project
  { -- | An identifier
    projId :: Id,
    -- | The name of the project
    projName :: Text,
    -- | The list of its dependencies
    projDependencies :: [Id]
  }
  deriving (Show)
