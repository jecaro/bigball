module Project (Id(..), Project(..), Projects, subProject)
    where

import Relude 

import qualified Data.Map.Strict as Map

newtype Id = Id { unId :: Text }
    deriving (Eq, Ord, Show)

data Project = Project
    { projName :: Text
    , projDependencies :: [ Id ]
    }
    deriving Show


type Projects = Map Id Project


subProject :: Id -> Projects -> Projects
subProject id_ projects = 
    case Map.lookup id_ projects of
        Nothing -> Map.empty
        Just p@(Project _ deps) -> 
            let mapWithOnlyProject = Map.singleton id_ p
                projDeps = catMaybes $ (`Map.lookup` projects) <$> deps
            in Map.union mapWithOnlyProject $ Map.fromList $ zip deps projDeps 
