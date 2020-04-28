module JSGraph (nodesAndEdges)
    where

import Relude 

import qualified Data.Text as T

import Graph


nodesAndEdges :: Graph -> Text
nodesAndEdges graph =  nodesJs (vertices graph) <> edgesJs (edges graph)


node :: Vertex -> Text
node (Vertex i name) = "{id: " <> show i <> ", label: '" <> name <> "'}"


nodesJs :: [Vertex] -> Text
nodesJs v = 
    let defs = T.intercalate ",\n" $ node <$> v
    in "var nodes = [\n" <> defs <> "]\n"


edge :: Edge -> Text
edge (Vertex f _, Vertex t _) = "{from: " <> show f <> ", to: " <> show t <> "}"


edgesJs :: [Edge] -> Text
edgesJs e = 
    let defs = T.intercalate ",\n" $ edge <$> e
    in "var edges = [\n" <> defs <> "]\n"


