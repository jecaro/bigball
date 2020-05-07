-- | The main module
{-# LANGUAGE TemplateHaskell #-}

import Relude

import Path
    ( Abs
    , Dir
    , File
    , Path
    , SomeBase(..)
    , fromSomeFile
    , mkRelFile
    , parent
    , toFilePath
    , (</>)
    )
import Path.IO (ensureDir, getCurrentDir)
import Text.Parsec.Text (parseFromFile)

import Filenames (allGraphJs, fullGraphJs, level1GraphJs)
import Graph
    ( Graph
    , Vertex
    , fromVertexFull
    , fromVertexLevel1
    , projectFromVertex
    , reverseDependenciesFull
    , reverseDependenciesLevel1
    , vertices
    )
import HtmlFiles (indexHtml, projectHtml)
import JsVariable (nodesAndEdges, reverseJs)
import Options (Options(..), runCliParser)
import Parser (parseFile)
import Project (Project(..))


-- | The main function
main :: IO ()
main = runCliParser parseInputAndWriteToOuput

-- IO functions


-- | Write some 'Text' to a file creating intermediate directories if needed
writeFileTextPath :: MonadIO m => Path a File -> Text -> m ()
writeFileTextPath file text = do
    ensureDir $ parent file
    writeFileText (toFilePath file) text


-- | Write a graph to a file
writeFileGraph :: Path a File -> Graph -> IO ()
writeFileGraph file graph =
    writeFileTextPath file $ nodesAndEdges graph


-- | Write a graph and reverse dependencies to a file
writeFileGraphAndReverseDeps :: Path a File -> Graph -> [Vertex] -> IO ()
writeFileGraphAndReverseDeps file graph revDeps =
    writeFileTextPath file $ nodesAndEdges graph <> reverseJs revDeps


-- | Write the projects in a 'Graph' into a directory. It consists in
-- - an index.html file
-- - a project.html file
-- - for each project: a graph with its reverse dependency in two versions:
--   - the first level dependencies
--   - the full graph with all hidden dependencies
writeProjectsIn :: Graph -> Path Abs Dir -> IO()
writeProjectsIn graph outputDir = do
    -- Write the all graph
    filenameAll <- allGraphJs
    writeFileGraph (outputDir </> filenameAll) graph

    -- For all the project write the first level and the full graph
    for_ (vertices graph) $ \v -> do
        let name = projName $ projectFromVertex graph v
            level1 = fromVertexLevel1 graph v
            revDepLevel1 = reverseDependenciesLevel1 graph v

        filenameLevel1 <- level1GraphJs name
        writeFileGraphAndReverseDeps (outputDir </> filenameLevel1) level1 revDepLevel1

        let full = fromVertexFull graph v
            revDepFull = reverseDependenciesFull graph v

        filenameFull <- fullGraphJs name
        writeFileGraphAndReverseDeps (outputDir </> filenameFull) full revDepFull

    -- Write the html files
    writeFileTextPath (outputDir </> $(mkRelFile "project.html")) projectHtml
    writeFileTextPath (outputDir </> $(mkRelFile "index.html")) $ indexHtml graph


-- | Convert 'SomeBase' to an absolute path prepending the current directory if
-- needed
withCurrentDir :: SomeBase Dir -> IO (Path Abs Dir)
withCurrentDir (Abs path) = pure path
withCurrentDir (Rel path) = getCurrentDir <&> (</> path)


-- | The command interpreter function
parseInputAndWriteToOuput :: Options -> IO ()
parseInputAndWriteToOuput (Options inputFile outputDir) = do
    -- Parse the file
    res <- parseFromFile parseFile (fromSomeFile inputFile)
    case res of
        Left err -> putTextLn $ show err
        Right projects -> writeProjectsIn projects =<< withCurrentDir outputDir

