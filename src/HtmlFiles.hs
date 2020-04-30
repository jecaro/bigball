{-# LANGUAGE TemplateHaskell #-}
module HtmlFiles (projectHtml, indexHtml)
    where

import Relude

import Data.FileEmbed
import Lucid

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
                    aForGraph allGraph allGraph
                    table_ [ class_ "table" ] $ do
                        thead_ $
                            tr_ $ do
                                th_ "Name"
                                th_ "Direct graph"
                                th_ "Indirect graph"
                        tbody_ $
                            traverse_ trForVertex (vertices graph)

  where
    aForGraph :: Text -> Text -> Html ()
    aForGraph name text = a_ [ href_ $ "project.html?name=" <> name ] $ toHtml text

    trForVertex :: Vertex -> Html ()
    trForVertex v =
        let (Project _ name _) = projectFromVertex graph v
            nbVertices g = length (vertices g) - 1
            direct = nbVertices $ fromVertexDirect graph v
            indirect = nbVertices $ fromVertexIndirect graph v
        in
            tr_ $ do
                td_ $ toHtml name
                td_ $ aForGraph (directGraph name) (show direct)
                td_ $ aForGraph (indirectGraph name) (show indirect) 

