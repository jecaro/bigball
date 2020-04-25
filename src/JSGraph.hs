module JSGraph (nodesAndEdges)
    where

import Relude 
import Relude.Extra.Map

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
    let indexes = Map.toList $ projectsToIndexes projects
        defs = T.intercalate ",\n" $ concat $ uncurry edge <$> indexes
    in "var edges = [\n" <> defs <> "]\n"


