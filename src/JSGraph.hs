module JSGraph (nodesAndEdges)
    where

import Relude hiding ((<|>), many)
import Relude.Extra.Map

import Data.List
import qualified Data.Map.Strict as Map
import qualified Data.Text as T

import Project


nodesAndEdges :: Projects -> Text
nodesAndEdges projects = nodes projects <> edges projects


node :: Int -> Text -> Text
node i name = "{id: " <> show i <> ", label: '" <> name <> "'}"


nodes :: Projects -> Text
nodes projects =
    let names = projName <$> elems projects
        idAndNames = zip [0::Int ..] names
        defs = T.intercalate ",\n" $ uncurry node <$> idAndNames
    in "var nodes = [\n" <> defs <> "]\n"


edgeFromTo :: Int -> Int -> Text
edgeFromTo f t = "{from: " <> show f <> ", to: " <> show t <> "}"


edge :: Int -> [Int] -> [Text]
edge project dependencies =
    let projectAndDependencies = zip (repeat project) dependencies
    in uncurry edgeFromTo <$> projectAndDependencies


edges :: Projects -> Text
edges projects =
    let indexes = Map.toList $ mapProjectsToIndexes projects
        defs = T.intercalate ",\n" $ concat $ uncurry edge <$> indexes
    in "var edges = [\n" <> defs <> "]\n"


mapProjectsToIndexes :: Projects -> Map Int [Int]
mapProjectsToIndexes projects =
        -- Map a project id to int id
    let intIdFromId identifier = elemIndex identifier $ keys projects
        -- Take the dependencies of a project and get a list of int ids
        projToInds (Project _ deps) = catMaybes $ intIdFromId <$> deps
        -- Convert the keys to int ids
        mapWithIndsAsDep = projToInds <$> projects
        -- Fold function to create the output map
        foldFct :: Text -> [Int] -> Map Int [Int] -> Map Int [Int]
        foldFct k v m =
            case intIdFromId k of
                Nothing -> m
                Just identifier -> Map.insert identifier v m
    in Map.foldrWithKey foldFct Map.empty mapWithIndsAsDep

