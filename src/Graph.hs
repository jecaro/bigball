module Graph 
    ( Graph(..)
    , Edge
    , Vertex(..)
    , edges
    , fromProjects
    , fromVertex
    , projectFromVertex
    , vertices
    )
    where

import Relude

import qualified Data.Graph as G
import qualified Data.Set as Set

import Project


data Graph = Graph
    { grGraph :: G.Graph
    , grNodeFromVertex :: G.Vertex -> (Text, Id, [Id])
    , grVertextFromKey :: Id -> Maybe G.Vertex
    }


data Vertex = Vertex
    { veId :: G.Vertex -- Int
    , veName :: Text
    }
    deriving (Eq, Ord, Show)


type Edge = (Vertex, Vertex)


fromProjects :: [Project] -> Graph
fromProjects projects = Graph graph nodeFromVertex vertexFromKey
  where
    projectToTuple (Project id_ name deps) = (name, id_, deps)
    (graph, nodeFromVertex, vertexFromKey) = G.graphFromEdges $ projectToTuple <$> projects 


tupleToList :: [(a, a)] -> [a]
tupleToList ((e1, e2):xs) = e1 : e2 : tupleToList xs
tupleToList _ = []


fromVertex :: Graph -> Vertex -> Graph
fromVertex (Graph graph nodeFromVertex _) vertex = 
    let gVertex = veId vertex
        -- Get edges starting from this vertex
        e = filter ((==) gVertex . fst) $ G.edges graph
        -- Get unique vertices for these edges along with the vertex
        v = Set.toList $ Set.fromList $ gVertex : tupleToList e
        -- And the related nodes
        n = nodeFromVertex <$> v
        -- Get the ids of the nodes
        ids = (\(_, id_, _) -> id_) <$> n
        keepIfInIds = filter (`elem` ids)
        -- Filters the dependencies to keep only these ids
        projects = 
            (\(name, id_, deps) -> Project id_ name (keepIfInIds deps)) <$> n
    in fromProjects projects


gVertexToVertex :: (G.Vertex -> (Text, Id, [Id])) -> G.Vertex -> Vertex
gVertexToVertex nodeFromVertex vertex = Vertex vertex name
    where (name, _, _) = nodeFromVertex vertex


vertices :: Graph -> [Vertex]
vertices (Graph graph nodeFromVertex _) = 
    gVertexToVertex nodeFromVertex <$> G.vertices graph


projectFromVertex :: Graph -> Vertex -> Project
projectFromVertex (Graph _ nodeFromVertex _) v = 
    let gv = veId v
        (name, id_, ids) = nodeFromVertex gv
    in Project id_ name ids


gEdgeToEdge :: (G.Vertex -> (Text, Id, [Id])) -> G.Edge -> Edge
gEdgeToEdge nodeFromVertex (e1, e2) = 
    let convertVertex = gVertexToVertex nodeFromVertex
    in (convertVertex e1, convertVertex e2)


edges :: Graph -> [Edge]
edges (Graph graph nodeFromVertex _) = gEdgeToEdge nodeFromVertex <$> G.edges graph
