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
                    traverse_ trForVertex (vertices graph)

  where
    aProject :: Text -> Html ()
    aProject name = a_ [ href_ $ "project.html?name=" <> name ] $ toHtml name

    trForVertex :: Vertex -> Html ()
    trForVertex v =
        let (Project _ name _) = projectFromVertex graph v
            nbVertices g = length (vertices g) - 1
            direct = nbVertices $ fromVertexDirect graph v
            indirect = nbVertices $ fromVertexIndirect graph v
        in
            tr_ $ do
                td_ $ aProject name
                td_ $ show direct
                td_ $ show indirect


