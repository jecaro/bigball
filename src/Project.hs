module Project (Id(..), Project(..), Projects, indirect, projectsToIndexes, subProject)
    where

import Relude 
import Relude.Extra.Map

import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set


newtype Id = Id { unId :: Text }
    deriving (Eq, Ord, Show)


data Project = Project
    { projName :: Text
    , projDependencies :: Set Id
    }
    deriving Show


type Projects = Map Id Project


-- | Get the dependencies of an 'Id' as a 'Projects'
subProject :: Id -> Projects -> Projects
subProject id_ projects = 
    case Map.lookup id_ projects of
        Nothing -> Map.empty
        Just p@(Project _ ids) -> 
            let project = Map.singleton id_ p
                dependencies = foldr f Map.empty ids 
            in Map.union project dependencies
  where
    f :: Id -> Projects -> Projects
    f currentId outProjects =
       case Map.lookup currentId projects of
           Nothing -> outProjects
           Just project -> Map.insert id_ project outProjects


-- | Compute the set of indirect dependencies
indirect :: Id -> Projects -> Set Id
indirect id_ projects =
    let maybeDeps :: Maybe (Set Id)
        maybeDeps = projDependencies <$> Map.lookup id_ projects
        dependencies :: Set Id
        dependencies = fromMaybe Set.empty maybeDeps 
    in case length dependencies of
         0 -> dependencies
         _ -> dependencies <> foldMap (`indirect` projects) dependencies
    

-- | Get the index of an element in the map
elemIndex :: Id -> Projects -> Maybe Int
elemIndex identifier projects = List.elemIndex identifier $ keys projects


-- | Convert 'Id' to 'Int' keeping the 'Map' structure
projectsToIndexes :: Projects -> Map Int [Int]
projectsToIndexes projects =
        -- Take the dependencies of a project and get a list of int ids
    let depsToIndexes (Project _ deps) = foldMap f deps
            where 
                f id_ = fromMaybe [] $ pure <$> elemIndex id_ projects
        -- Convert the keys to int ids
        mapWithIndsAsDep = depsToIndexes <$> projects
        result = Map.foldrWithKey f Map.empty mapWithIndsAsDep
            where
                f k v m =
                    case elemIndex k projects of
                        Nothing -> m
                        Just identifier -> Map.insert identifier v m
    in result
