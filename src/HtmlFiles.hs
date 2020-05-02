{-# LANGUAGE TemplateHaskell #-}
module HtmlFiles (projectHtml, indexHtml)
    where

import Relude

import Data.FileEmbed
import Lucid
import Path

import Graph
import Filenames
import Project


projectHtml :: Text
projectHtml = decodeUtf8 $(embedFile "data/project.html")


indexHtml :: Graph -> Text
indexHtml projects = toText . renderText $ index projects


index :: Graph -> Html ()
index graph =
    doctypehtml_ $ do
        head_ $ do
            meta_ [charset_ "utf-8"]
            meta_ [name_ "viewport", content_ "width=device-width, initial-scale=1"]
            link_ [rel_ "stylesheet", href_ "https://cdn.jsdelivr.net/npm/bulma@0.8.2/css/bulma.min.css"]
            title_ "Projects"
        body_ $ 
            section_ [ class_ "section" ] $
                div_ [ class_ "container" ] $ do
                    whenJust allGraphJs (\f -> aForGraph allGraph f allGraph)
                    table_ [ class_ "table" ] $ do
                        thead_ $
                            tr_ $ do
                                th_ "Name"
                                th_ "First level"
                                th_ "Full graph"
                        tbody_ $
                            traverse_ trForVertex (vertices graph)

  where
    aForGraph :: Text -> Path Rel File -> Text -> Html ()
    aForGraph name dataFile text = a_ [ href_ url ] $ toHtml text
      where url = "project.html?projName=" <> name <> "&dataFile=" 
                <> toText (toFilePath dataFile)

    trForVertex :: Vertex -> Html ()
    trForVertex v =
        let (Project _ name _) = projectFromVertex graph v
            nbVertices g = length (vertices g) - 1
            level1 = nbVertices $ fromVertexLevel1 graph v
            full = nbVertices $ fromVertexFull graph v
        in
            tr_ $ do
                td_ $ toHtml name
                td_ $ whenJust (level1GraphJs name) 
                    (\f -> aForGraph name f (show level1))
                td_ $ whenJust (fullGraphJs name) 
                    (\f -> aForGraph name f (show full))

