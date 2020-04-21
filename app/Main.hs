{-# LANGUAGE TemplateHaskell #-}

import Relude hiding ((<|>), many)

import qualified Data.Map.Strict as Map
import Path
import Path.IO
import Text.Parsec.Text

import JSGraph
import Options
import Parser
import Project

main :: IO ()
main = runCliParser untangle

-- IO functions

stringToPath :: String -> IO (Path Abs Dir)
stringToPath outDirStr@('/':_) = parseAbsDir outDirStr
stringToPath outDirStr = do
    currentDir <- getCurrentDir
    outDir <- parseRelDir outDirStr
    pure $ currentDir </> outDir


writeGraph :: Projects -> Path a File -> IO ()
writeGraph projects file = do
    ensureDir $ parent file
    writeFileText (toFilePath file) $ nodesAndEdges projects 


untangle :: Options -> IO ()
untangle (Options inputFileStr outputDirStr) = do
    res <- parseFromFile parseFile inputFileStr
    case res of
        Left err -> putTextLn $ show err
        Right projects -> do
            outputDir <- stringToPath outputDirStr 
            writeGraph projects (outputDir </> $(mkRelFile "all.js"))
            for_ (Map.toList projects) $ \(id_, Project name _) -> do
                let sub = subProject id_ projects
                fileName <- parseRelFile $ toString name <> ".js"
                writeGraph sub (outputDir </> fileName)

