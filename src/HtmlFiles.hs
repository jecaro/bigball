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
    html_ $ do
        head_ $
            title_ "Projects"
        body_ $ do
            aForGraph allGraph allGraph
            table_ $ do
                thead_ $
                    tr_ $ do
                        th_ "Name"
                        th_ "Direct dependencies"
                        th_ "Indirect dependencies"
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

