{-# LANGUAGE TemplateHaskell #-}
module Files (projectHtml, indexHtml)
    where

import Relude

import Data.FileEmbed
import qualified Data.Map.Strict as Map
import Lucid

import Project


projectHtml :: Text
projectHtml = decodeUtf8 $(embedFile "data/project.html")


indexHtml :: Projects -> Text
indexHtml projects = toText . renderText $ index projects


index :: Projects -> Html ()
index projects = 
    let aProject :: Text -> Html ()
        aProject name = a_ [ href_ $ "project.html?name=" <> name ] $ toHtml name
        trProject :: Id -> Project -> Html ()
        trProject identifier project = 
            tr_ $ do
                td_ . aProject $ projName project
                td_ . show . length $ projDependencies project
                td_ . show . length $ indirect identifier projects
                
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
                            th_ "Direct dependencies"
                            th_ "Indirect dependencies"
                    tbody_ $
                        Relude.for_ (Map.toList projects) $ \(identifier, project) -> 
                            trProject identifier project
