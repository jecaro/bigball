import Relude

import Test.Hspec

import Project


project1 :: Project
project1 = Project (Id "1") "project 1" $ fromList $ Id <$> [ "2", "3" ]


project2 :: Project
project2 = Project (Id "2") "project 2" $ fromList $ Id <$> [ "3", "4" ]


project3 :: Project
project3 = Project (Id "3") "project 3" $ fromList $ Id <$> [ "5" ]


project4 :: Project
project4 = Project (Id "4") "project 4" $ fromList $ Id <$> []


project5 :: Project
project5 = Project (Id "5") "project 5" $ fromList $ Id <$> []


projects :: [Project]
projects = fromList [project1, project2, project3, project4]


testProject :: Spec
testProject = 
    describe "Project" $
        it "Test the function indirect" $ do
            pending
            --indirect (Id "4") projects `shouldBe` Set.empty 
            --indirect (Id "5") projects `shouldBe` Set.empty 
            --indirect (Id "3") projects `shouldBe` Set.singleton (Id "5")
            --indirect (Id "2") projects `shouldBe` fromList (Id <$> [ "3", "4", "5"]) 
            --indirect (Id "1") projects `shouldBe` fromList (Id <$> [ "2", "3", "4", "5"]) 


main :: IO ()
main = hspec testProject
