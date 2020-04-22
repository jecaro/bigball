{-# LANGUAGE TemplateHaskell #-}
module Files (projectHtml, indexHtml)
    where

import Relude

import Data.FileEmbed
import Lucid


projectHtml :: Text
projectHtml = decodeUtf8 $(embedFile "data/project.html")


indexHtml :: [Text] -> Text
indexHtml names = toText $ renderText $ index names


index :: [Text] -> Html ()
index names = 
    let sortedNames = sort $ "all" : names 
        aProject :: Text -> Html ()
        aProject name = a_ [ href_ $ "project.html?name=" <> name ] $ toHtml name
    in 
        html_ $ do
            head_ $ 
                title_ ""
            body_ $ 
                ul_ $ 
                    foldMap (li_ . aProject) sortedNames
