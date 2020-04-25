import Relude

import qualified Data.Set as Set
import Test.Hspec

import Project


project1 :: (Id, Project)
project1 = (Id "1", Project "project 1" $ fromList $ Id <$> [ "2", "3" ])


project2 :: (Id, Project)
project2 = (Id "2", Project "project 2" $ fromList $ Id <$> [ "3", "4" ])


project3 :: (Id, Project)
project3 = (Id "3", Project "project 3" $ fromList $ Id <$> [ "5" ])


project4 :: (Id, Project)
project4 = (Id "4", Project "project 4" $ fromList $ Id <$> [])


project5 :: (Id, Project)
project5 = (Id "5", Project "project 5" $ fromList $ Id <$> [])


projects :: Projects
projects = fromList [project1, project2, project3, project4]


testProject :: Spec
testProject = 
    describe "Project" $
        it "Test the function indirect" $ do
            indirect (Id "4") projects `shouldBe` Set.empty 
            indirect (Id "5") projects `shouldBe` Set.empty 
            indirect (Id "3") projects `shouldBe` Set.singleton (Id "5")
            indirect (Id "2") projects `shouldBe` fromList (Id <$> [ "3", "4", "5"]) 
            indirect (Id "1") projects `shouldBe` fromList (Id <$> [ "2", "3", "4", "5"]) 


main :: IO ()
main = hspec testProject
