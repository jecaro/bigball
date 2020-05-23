-- | The javascript variables to be written in the data file. The variables are
-- set up to be used by visjs.
module JsVariable (nodesAndEdges, reverseJs) where

import qualified Data.Text as T
import Graph (Edge, Graph, Vertex (..), edges, vertices)
import Relude

-- | Two variables containing the graph
nodesAndEdges :: Graph -> Text
nodesAndEdges graph = nodesJs (vertices graph) <> edgesJs (edges graph)

-- | A single vertex
node :: Vertex -> Text
node (Vertex i name) = "{id: " <> show i <> ", label: '" <> name <> "'}"

-- | The variable containing the vertices
nodesJs :: [Vertex] -> Text
nodesJs v =
    let defs = T.intercalate ",\n" $ node <$> v
     in "const nodes = [\n" <> defs <> "]\n"

-- | Put a vertex name in a js string
vertexToText :: Vertex -> Text
vertexToText (Vertex _ name) = "'" <> name <> "'"

-- | The reverse dependencies
reverseJs :: [Vertex] -> Text
reverseJs v =
    let defs = T.intercalate ",\n" $ vertexToText <$> v
     in "const reverse = [\n" <> defs <> "]\n"

-- | An edge
edge :: Edge -> Text
edge (Vertex f _, Vertex t _) = "{from: " <> show f <> ", to: " <> show t <> "}"

-- | The variable containing the edges
edgesJs :: [Edge] -> Text
edgesJs e =
    let defs = T.intercalate ",\n" $ edge <$> e
     in "const edges = [\n" <> defs <> "]\n"
