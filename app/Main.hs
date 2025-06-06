{-# LANGUAGE TemplateHaskell #-}

-- | The main module
module Main where

import Control.Exception.Base (IOException)
import Control.Monad.Trans.Except.Extra (firstExceptT, handleIOExceptT)
import Data.Text.IO (hPutStrLn)
import Filenames (allGraphJs, fullGraphJs, level1GraphJs)
import Graph
  ( Graph,
    Vertex,
    fromVertexFull,
    fromVertexLevel1,
    projectFromVertex,
    reverseDependenciesFull,
    reverseDependenciesLevel1,
    vertices,
  )
import HtmlFiles (indexHtml, projectHtml)
import JsVariable (nodesAndEdges, reverseJs)
import qualified Options
import qualified Parser
import Path
  ( Abs,
    Dir,
    File,
    Path,
    SomeBase (..),
    mkRelFile,
    parent,
    toFilePath,
    (</>),
  )
import Path.IO (ensureDir, getCurrentDir)
import Project (Project (..))
import Relude
import System.Environment (getProgName)
import Text.Parsec (ParseError, parse)

-- | All the things that might go wrong
data Error
  = EParse ParseError
  | EUTF8Decode UnicodeException
  | EOptions Options.Error
  | ECreateDir Text IOException
  | EReadFile Text IOException
  | EWriteFile Text IOException

-- | The main function
main :: IO ()
main = do
  progName <- getProgName
  args <- getArgs
  -- Execute the program
  res <- runExceptT $ do
    options <-
      firstExceptT EOptions . hoistEither $ Options.parse progName args
    parseInputAndWriteToOuput options
  -- Handle error
  whenLeft_ res $ \e -> do
    hPutStrLn stderr $ render e
    exitFailure

-- | Render the error message
render :: Error -> Text
render (EOptions eOptions) = Options.render eOptions
render (EParse parseError) = Parser.render parseError
render (EUTF8Decode e) = "Error decoding UTF-8: " <> show e
render (ECreateDir filename e) =
  "Error creating directory '" <> toText filename <> "' : " <> show e
render (EReadFile filename e) =
  "Error reading '" <> toText filename <> "' : " <> show e
render (EWriteFile filename e) =
  "Error writing '" <> toText filename <> "' : " <> show e

-- IO functions

-- | Write some 'Text' to a file creating intermediate directories if needed
writeFileTextPath :: Path a File -> Text -> ExceptT Error IO ()
writeFileTextPath file text = do
  handleIOExceptT (ECreateDir dirnameText) $ ensureDir $ parent file
  handleIOExceptT (EWriteFile filenameText) $ writeFileText filename text
  where
    filename = toFilePath file
    filenameText = toText filename
    dirnameText = toText $ toFilePath $ parent file

-- | Write a graph to a file
writeFileGraph :: Path a File -> Graph -> ExceptT Error IO ()
writeFileGraph file graph = writeFileTextPath file $ nodesAndEdges graph

-- | Write a graph and reverse dependencies to a file
writeFileGraphAndReverseDeps ::
  Path a File ->
  Graph ->
  [Vertex] ->
  ExceptT Error IO ()
writeFileGraphAndReverseDeps file graph revDeps =
  writeFileTextPath file $ nodesAndEdges graph <> reverseJs revDeps

-- | Write the projects in a 'Graph' into a directory. It consists in
-- - an index.html file
-- - a project.html file
-- - for each project: a graph with its reverse dependency in two versions:
--   - the first level dependencies
--   - the full graph with all hidden dependencies
writeProjectsIn :: Graph -> Path Abs Dir -> ExceptT Error IO ()
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
withCurrentDir :: (MonadIO m) => SomeBase a -> m (Path Abs a)
withCurrentDir (Abs path) = pure path
withCurrentDir (Rel path) = getCurrentDir <&> (</> path)

-- | Actual processing function
parseInputAndWriteToOuput :: Options.Options -> ExceptT Error IO ()
parseInputAndWriteToOuput (Options.Options inputFile outputDir) = do
  -- Parse the solution file
  inputFileAbs <- liftIO $ withCurrentDir inputFile
  projects <- parseSlnFile inputFileAbs
  -- And write the result in the output file
  outputDirAbs <- withCurrentDir outputDir
  writeProjectsIn projects outputDirAbs

-- | Parse the solution file
parseSlnFile :: Path Abs File -> ExceptT Error IO Graph
parseSlnFile slnFile = do
  bs <- handleIOExceptT (EReadFile $ toText filename) $ readFileBS filename
  text <- firstExceptT EUTF8Decode . hoistEither $ decodeUtf8Strict bs
  firstExceptT EParse . hoistEither $ parse Parser.graph filename text
  where
    filename = toFilePath slnFile
