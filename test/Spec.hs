-- | The test suite
module Main where

import Data.Array (array)
import qualified Data.Graph as G
import Graph (Graph (..), Vertex (..), fromProjects, fromVertexFull, fromVertexLevel1)
import Project (Id (..), Project (..))
import Relude
import Test.Hspec (Spec, describe, hspec, it, shouldBe)

oneNodeNoEdge :: G.Graph
oneNodeNoEdge = array (0, 0) [(0, [])]

twoNodesNoEdge :: G.Graph
twoNodesNoEdge = array (0, 1) [(0, []), (1, [])]

project1 :: Project
project1 = Project (Id "1") "project 1" $ fromList $ Id <$> ["2", "3"]

project2 :: Project
project2 = Project (Id "2") "project 2" $ fromList $ Id <$> ["3", "4"]

project3 :: Project
project3 = Project (Id "3") "project 3" $ fromList $ Id <$> ["5"]

project4 :: Project
project4 = Project (Id "4") "project 4" $ fromList $ Id <$> []

project5 :: Project
project5 = Project (Id "5") "project 5" $ fromList $ Id <$> []

graph :: Graph
graph = fromProjects [project1, project2, project3, project4, project5]

testGraph :: Spec
testGraph = do
    describe "Graph creation" $ do
        it "Test the function fromProject with one node" $ do
            grGraph (fromProjects [project1]) `shouldBe` oneNodeNoEdge
            grGraph (fromProjects [project2]) `shouldBe` oneNodeNoEdge
            grGraph (fromProjects [project3]) `shouldBe` oneNodeNoEdge
            grGraph (fromProjects [project4]) `shouldBe` oneNodeNoEdge
            grGraph (fromProjects [project5]) `shouldBe` oneNodeNoEdge
        it "Test the function fromProject with more nodes but no edges" $ do
            grGraph (fromProjects [project1, project4]) `shouldBe` twoNodesNoEdge
            grGraph (fromProjects [project1, project5]) `shouldBe` twoNodesNoEdge
        it "Test the function fromProject with nodes and edges" $
            grGraph (fromProjects [project1, project2, project3])
                `shouldBe` array (0, 2) [(0, [1, 2]), (1, [2]), (2, [])]
    describe "Subgraphs" $ do
        it "Test getting the first level" $
            grGraph (fromVertexLevel1 graph (Vertex 0 "1"))
                `shouldBe` array (0, 2) [(0, [1, 2]), (1, [2]), (2, [])]
        it "Test getting all the levels" $
            grGraph (fromVertexFull graph (Vertex 0 "1"))
                `shouldBe` array
                    (0, 4)
                    [ (0, [1, 2]),
                      (1, [2, 3]),
                      (2, [4]),
                      (3, []),
                      (4, [])
                    ]

main :: IO ()
main = hspec testGraph
