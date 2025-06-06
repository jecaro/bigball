{-# LANGUAGE TemplateHaskell #-}

-- | The HTML files to be written in 'Text'
module HtmlFiles (projectHtml, indexHtml) where

import Data.FileEmbed (embedFile)
import Filenames (allGraph, allGraphJs, fullGraphJs, level1GraphJs)
import Graph
  ( Graph,
    Vertex (..),
    fromVertexFull,
    fromVertexLevel1,
    projectFromVertex,
    vertices,
  )
import Lucid
import Path (File, Path, Rel, toFilePath)
import Project (Project (..))
import Relude

-- | The project file
projectHtml :: Text
projectHtml = decodeUtf8 $(embedFile "data/project.html")

-- | The index file
indexHtml :: Graph -> Text
indexHtml projects = toText . renderText $ index projects

-- | The index file in HTML form
index :: Graph -> Html ()
index graph =
  doctypehtml_ $ do
    head_ $ do
      meta_ [charset_ "utf-8"]
      meta_ [name_ "viewport", content_ "width=device-width, initial-scale=1"]
      link_ [rel_ "stylesheet", href_ "https://cdn.jsdelivr.net/npm/bulma@0.8.2/css/bulma.min.css"]
      title_ "Projects"
    body_
      $ section_ [class_ "section"]
      $ div_ [class_ "container"]
      $ do
        whenJust allGraphJs (\f -> aForGraph allGraph f allGraph)
        table_ [class_ "table"] $ do
          thead_
            $ tr_
            $ do
              th_ "Name"
              th_ "First level"
              th_ "Full graph"
          tbody_
            $ traverse_ trForVertex
            $ sortBy compareNames (vertices graph)
  where
    compareNames :: Vertex -> Vertex -> Ordering
    compareNames Vertex {veName = n1} Vertex {veName = n2} = compare n1 n2
    aForGraph :: Text -> Path Rel File -> Text -> Html ()
    aForGraph name dataFile text = a_ [href_ url] $ toHtml text
      where
        url =
          "project.html?projName="
            <> name
            <> "&dataFile="
            <> toText (toFilePath dataFile)
    trForVertex :: Vertex -> Html ()
    trForVertex v =
      let (Project _ name _) = projectFromVertex graph v
          nbVertices g = length (vertices g) - 1
          level1 = nbVertices $ fromVertexLevel1 graph v
          full = nbVertices $ fromVertexFull graph v
       in tr_ $ do
            td_ $ toHtml name
            td_
              $ whenJust
                (level1GraphJs name)
                (\f -> aForGraph name f (show level1))
            td_
              $ whenJust
                (fullGraphJs name)
                (\f -> aForGraph name f (show full))
