{-# LANGUAGE TemplateHaskell #-}

import Relude 

import Path
import Path.IO
import Text.Parsec.Text

import Files
import Graph
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


writeFileProjects :: Path a File -> Graph -> IO ()
writeFileProjects file graph = 
    writeFileTextPath file $ nodesAndEdges graph


writeProjectsIn :: Graph -> Path Abs Dir -> IO()
writeProjectsIn graph outputDir = do
    -- Write the all graph
    writeFileProjects (outputDir </> $(mkRelFile "all.js")) graph
    -- For all the project write the graph of the subdir
    for_ (vertices graph) $ \v -> do
        let sub = fromVertexDirect graph v
            name = projName $ projectFromVertex graph v
        fileName <- parseRelFile $ toString name <> ".js"
        writeFileProjects (outputDir </> fileName) sub
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
