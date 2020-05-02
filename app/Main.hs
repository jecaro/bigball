{-# LANGUAGE TemplateHaskell #-}

import Relude 

import Path
import Path.IO
import Text.Parsec.Text

import Filenames
import Graph
import HtmlFiles
import JSGraph
import Options
import Parser
import Project


main :: IO ()
main = runCliParser parseInputAndWriteToOuput

-- IO functions

stringToPath :: String -> IO (Path Abs Dir)
stringToPath outDirStr@('/':_) = parseAbsDir outDirStr
stringToPath outDirStr = do
    currentDir <- getCurrentDir
    outDir <- parseRelDir outDirStr
    pure $ currentDir </> outDir


writeFileTextPath :: MonadIO m => Path a File -> Text -> m ()
writeFileTextPath file text = do
    ensureDir $ parent file
    writeFileText (toFilePath file) text


writeFileGraph :: Path a File -> Graph -> IO ()
writeFileGraph file graph = 
    writeFileTextPath file $ nodesAndEdges graph


writeFileGraphAndReverseDeps :: Path a File -> Graph -> [Vertex] -> IO ()
writeFileGraphAndReverseDeps file graph revDeps = 
    writeFileTextPath file $ nodesAndEdges graph <> reverseJs revDeps


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


parseInputAndWriteToOuput :: Options -> IO ()
parseInputAndWriteToOuput (Options inputFileStr outputDirStr) = do
    -- Parse the file
    res <- parseFromFile parseFile inputFileStr
    case res of
        Left err -> putTextLn $ show err
        Right projects -> do
            outputDir <- stringToPath outputDirStr 
            writeProjectsIn projects outputDir
