-- | Main structure to store the graph of dependencies of a projet
module Graph
    ( Graph(..)
    , Edge
    , Vertex(..)
    , edges
    , fromProjects
    , fromVertexFull
    , fromVertexLevel1
    , projectFromVertex
    , reverseDependenciesFull
    , reverseDependenciesLevel1
    , vertices
    )
    where

import Relude

import qualified Data.Graph as G
import qualified Data.Set as Set

import Project (Project(..), Id(..))


-- | Structure to store the graph, reflect the output of 'G.graphFromEdges'
data Graph = Graph
    { grGraph :: G.Graph
    , grNodeFromVertex :: G.Vertex -> (Text, Id, [Id])
    , grVertextFromKey :: Id -> Maybe G.Vertex
    }


-- | Exposed 'Vertex' type
data Vertex = Vertex
    { veId :: G.Vertex -- ^ The identifier in 'G.Graph'
    , veName :: Text   -- ^ The name of the project
    }
    deriving (Eq, Ord, Show)


-- | Exposed 'Edge' type
type Edge = (Vertex, Vertex)


-- | Create a 'Graph' from a list of 'Project'
fromProjects :: [Project] -> Graph
fromProjects projects = Graph graph nodeFromVertex vertexFromKey
  where
    projectToTuple (Project id_ name deps) = (name, id_, deps)
    (graph, nodeFromVertex, vertexFromKey) = G.graphFromEdges $ projectToTuple <$> projects


-- | Flatten a list of 2-tuples
tupleToList :: [(a, a)] -> [a]
tupleToList ((e1, e2):xs) = e1 : e2 : tupleToList xs
tupleToList _ = []


-- | Given a 'Vertex' and a 'Graph' returns the graph composed by the vertex
-- and all its direct descendants
fromVertexLevel1 :: Graph -> Vertex -> Graph
fromVertexLevel1 g@(Graph graph _ _) vertex =
    let gVertex = veId vertex
        -- Get edges starting from this vertex
        e = filter ((==) gVertex . fst) $ G.edges graph
        -- Get unique vertices for these edges along with the vertex
        v = Set.toList $ Set.fromList $ gVertex : tupleToList e
    in fromGVertexList g v


-- | Given a 'Vertex' and a 'Graph' returns the graph composed by the vertex
-- and all its descendants
fromVertexFull :: Graph -> Vertex -> Graph
fromVertexFull g@(Graph graph _ _) vertex =
    let gVertex = veId vertex
        v = G.reachable graph gVertex
    in fromGVertexList g v


-- | Convert a list of 'G.Vertex' to a Graph
fromGVertexList :: Graph -> [G.Vertex] -> Graph
fromGVertexList (Graph _ nodeFromVertex _) v =
    let n = nodeFromVertex <$> v
        -- Get the ids of the nodes
        ids = (\(_, id_, _) -> id_) <$> n
        keepIfInIds = filter (`elem` ids)
        -- Filters the dependencies to keep only these ids
        projects =
            (\(name, id_, deps) -> Project id_ name (keepIfInIds deps)) <$> n
    in fromProjects projects


-- | Convert a 'G.Vertex' to a 'Vertex'
gVertexToVertex :: (G.Vertex -> (Text, Id, [Id])) -> G.Vertex -> Vertex
gVertexToVertex nodeFromVertex vertex = Vertex vertex name
    where (name, _, _) = nodeFromVertex vertex


-- | Returns the vertices of a 'Graph'
vertices :: Graph -> [Vertex]
vertices (Graph graph nodeFromVertex _) =
    gVertexToVertex nodeFromVertex <$> G.vertices graph


-- | Construct a 'Project' from a 'Vertex'
projectFromVertex :: Graph -> Vertex -> Project
projectFromVertex (Graph _ nodeFromVertex _) v =
    let gv = veId v
        (name, id_, ids) = nodeFromVertex gv
    in Project id_ name ids


-- | Convert a 'G.Edge' to an 'Edge'
gEdgeToEdge :: (G.Vertex -> (Text, Id, [Id])) -> G.Edge -> Edge
gEdgeToEdge nodeFromVertex (e1, e2) =
    let convertVertex = gVertexToVertex nodeFromVertex
    in (convertVertex e1, convertVertex e2)


-- | Returns the edges of a 'Graph'
edges :: Graph -> [Edge]
edges (Graph graph nodeFromVertex _) = gEdgeToEdge nodeFromVertex <$> G.edges graph


-- | Get the vertices with a direct dependency of a 'Vertex'
reverseDependenciesLevel1 :: Graph -> Vertex -> [Vertex]
reverseDependenciesLevel1 g@(Graph graph _ _) v =
    let reverseGraph = g{grGraph = G.transposeG graph}
        level1 = fromVertexLevel1 reverseGraph v
    in filter ((/=) (veName v) . veName) $ vertices level1


-- | Get the vertices with a dependency of a 'Vertex'
reverseDependenciesFull :: Graph -> Vertex -> [Vertex]
reverseDependenciesFull g@(Graph graph _ _) v =
    let reverseGraph = g{grGraph = G.transposeG graph}
        full = fromVertexFull reverseGraph v
    in filter ((/=) (veName v) . veName) $ vertices full

