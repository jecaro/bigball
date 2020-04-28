{-# LANGUAGE TemplateHaskell #-}
module Files (projectHtml, indexHtml)
    where

import Relude

import Data.FileEmbed
import Lucid

import Graph
import Project


projectHtml :: Text
projectHtml = decodeUtf8 $(embedFile "data/project.html")


indexHtml :: Graph -> Text
indexHtml projects = toText . renderText $ index projects


index :: Graph -> Html ()
index graph = 
    let aProject :: Text -> Html ()
        aProject name = a_ [ href_ $ "project.html?name=" <> name ] $ toHtml name
                
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
                    tbody_ $
                        Relude.for_ (projectFromVertex graph <$> vertices graph) 
                            (\(Project _ name d) -> 
                                tr_ $ do 
                                    td_ $ aProject name
                                    td_ $ show $ length d
                                ) 
                                
