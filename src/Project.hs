module Project (Project(..), Projects, subProject)
    where

import Relude 

import qualified Data.Map.Strict as Map


data Project = Project
    { projName :: Text
    , projDependencies :: [ Text ]
    }
    deriving Show


type Projects = Map Text Project


subProject :: Text -> Projects -> Projects
subProject id_ projects = 
    case Map.lookup id_ projects of
        Nothing -> Map.empty
        Just p@(Project _ deps) -> 
            let mapWithOnlyProject = Map.singleton id_ p
                projDeps = catMaybes $ (`Map.lookup` projects) <$> deps
            in Map.union mapWithOnlyProject $ Map.fromList $ zip deps projDeps 
