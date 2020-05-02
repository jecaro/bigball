module JSGraph (nodesAndEdges, reverseJs)
    where

import Relude

import qualified Data.Text as T

import Graph (Edge, Graph, Vertex(..), edges, vertices)


nodesAndEdges :: Graph -> Text
nodesAndEdges graph =  nodesJs (vertices graph) <> edgesJs (edges graph)


node :: Vertex -> Text
node (Vertex i name) = "{id: " <> show i <> ", label: '" <> name <> "'}"


nodesJs :: [Vertex] -> Text
nodesJs v =
    let defs = T.intercalate ",\n" $ node <$> v
    in "const nodes = [\n" <> defs <> "]\n"


var :: Vertex -> Text
var (Vertex _ name) = "'" <> name <> "'"

reverseJs :: [Vertex] -> Text
reverseJs v =
    let defs = T.intercalate ",\n" $ var <$> v
    in "const reverse = [\n" <> defs <> "]\n"


edge :: Edge -> Text
edge (Vertex f _, Vertex t _) = "{from: " <> show f <> ", to: " <> show t <> "}"


edgesJs :: [Edge] -> Text
edgesJs e =
    let defs = T.intercalate ",\n" $ edge <$> e
    in "const edges = [\n" <> defs <> "]\n"


