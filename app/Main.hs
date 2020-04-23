{-# LANGUAGE TemplateHaskell #-}

import Relude 

import qualified Data.Map.Strict as Map
import Path
import Path.IO
import Text.Parsec.Text

import Files
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


writeFileProjects :: Path a File -> Projects -> IO ()
writeFileProjects file projects = 
    writeFileTextPath file $ nodesAndEdges projects 


writeProjectsIn :: Projects -> Path Abs Dir -> IO()
writeProjectsIn projects outputDir = do
    -- Write the all graph
    writeFileProjects (outputDir </> $(mkRelFile "all.js")) projects
    -- For all the project write the graph of the subdir
    for_ (Map.toList projects) $ \(id_, Project name _) -> do
        let sub = subProject id_ projects
        fileName <- parseRelFile $ toString name <> ".js"
        writeFileProjects (outputDir </> fileName) sub
    -- Write the html files
    writeFileTextPath (outputDir </> $(mkRelFile "project.html")) projectHtml
    let indexHtml' = indexHtml $ Map.elems projects
    writeFileTextPath (outputDir </> $(mkRelFile "index.html")) indexHtml'


parseInputAndWriteToOuput :: Options -> IO ()
parseInputAndWriteToOuput (Options inputFileStr outputDirStr) = do
    -- Parse the file
    res <- parseFromFile parseFile inputFileStr
    case res of
        Left err -> putTextLn $ show err
        Right projects -> do
            outputDir <- stringToPath outputDirStr 
            writeProjectsIn projects outputDir
