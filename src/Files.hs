{-# LANGUAGE TemplateHaskell #-}
module Files (projectHtml, indexHtml)
    where

import Relude

import Data.FileEmbed
import Lucid

import Project


projectHtml :: Text
projectHtml = decodeUtf8 $(embedFile "data/project.html")


indexHtml :: [Project] -> Text
indexHtml names = toText $ renderText $ index names


index :: [Project] -> Html ()
index projects = 
    let sorted = sortOn projName projects 
        aProject :: Text -> Html ()
        aProject name = a_ [ href_ $ "project.html?name=" <> name ] $ toHtml name
        trProject :: Project -> Html ()
        trProject project = 
            tr_ $ do
                td_ $ aProject $ projName project
                td_ $ show $ length $ projDependencies project
                
    in 
        html_ $ do
            head_ $ 
                title_ "Projects"
            body_ $ do
                aProject "all"
                table_ $ do
                    thead_ $
                        tr_ $ do
                            th_ "Name"
                            th_ "Number of direct dependencies"
                    tbody_ $
                        foldMap trProject sorted
