{-# LANGUAGE TemplateHaskell #-}

import Relude hiding ((<|>), many)
import Relude.Extra.Map

import Data.List
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Options.Applicative as Opt
import Path
import Path.IO
import Text.Parsec
import Text.Parsec.Text


-- Command line

data Options = Options
  { optInputFile :: String
  , optOutputFile :: String
  }

options :: Opt.Parser Options
options = Options
      <$> Opt.strOption
          ( Opt.long "input"
         <> Opt.metavar "INPUT"
         <> Opt.help "Input file" )
      <*> Opt.strOption
          ( Opt.long "output"
         <> Opt.metavar "OUTPUT"
         <> Opt.help "Output file" )

main :: IO ()
main = Opt.execParser opts >>= untangle
  where opts = Opt.info (options <**> Opt.helper) Opt.idm

-- Main data structure

data Project = Project
    { projName :: Text
    , projDependencies :: [ Text ]
    }
    deriving Show

type Projects = Map Text Project

-- Parsing

skipTo :: Parser a -> Parser ()
skipTo p = void $ manyTill anyChar p

skipLine :: Parser ()
skipLine = skipTo endOfLine

projectNameAndId :: Parser (Text, Text)
projectNameAndId = do
    void $ string "Project" >> skipTo (string " = \"")
    name <- toText <$> many1 (alphaNum <|> char '_')
    void $ count 2 $ skipTo (string ", \"")
    identifier <- projectId
    skipLine
    pure (name, identifier)

projectEnd :: Parser ()
projectEnd = string "EndProject" *> skipLine

projectParser :: Parser (Text, Project)
projectParser
    = idAndProject
    <$> projectNameAndId
    <*> option [] section <* projectEnd
  where idAndProject (name, identifier) ids = (identifier, Project name ids)

sectionStart :: Parser ()
sectionStart = spaces >> string "ProjectSection" *> skipLine

sectionEnd :: Parser ()
sectionEnd = spaces >> string "EndProjectSection" *> skipLine

projectId :: Parser Text
projectId = between (char '{') (char '}') (toText <$> many (noneOf ['\n','}']))

sectionLine :: Parser Text
sectionLine = spaces >> projectId <* skipLine

section :: Parser [Text]
section = between sectionStart sectionEnd $ many (try sectionLine)

parseFile :: Parser Projects
parseFile = do
    _ <- count 4 skipLine
    projectsList <- many (try projectParser)
    pure $ fromList projectsList

-- Write JS graph

node :: Int -> Text -> Text
node i name = "{id: " <> show i <> ", label: '" <> name <> "'}"

nodes :: Projects -> Text
nodes projects =
    let names = projName <$> elems projects
        idAndNames = zip [0::Int ..] names
        defs = T.intercalate ",\n" $ uncurry node <$> idAndNames
    in "var nodes = [\n" <> defs <> "]\n"

edgeFromTo :: Int -> Int -> Text
edgeFromTo f t = "{from: " <> show f <> ", to: " <> show t <> "}"

edge :: Int -> [Int] -> [Text]
edge project dependencies =
    let projectAndDependencies = zip (repeat project) dependencies
    in uncurry edgeFromTo <$> projectAndDependencies

edges :: Projects -> Text
edges projects =
    let indexes = Map.toList $ mapProjectsToIndexes projects
        defs = T.intercalate ",\n" $ concat $ uncurry edge <$> indexes
    in "var edges = [\n" <> defs <> "]\n"

mapProjectsToIndexes :: Projects -> Map Int [Int]
mapProjectsToIndexes projects =
        -- Map a project id to int id
    let intIdFromId identifier = elemIndex identifier $ keys projects
        -- Take the dependencies of a project and get a list of int ids
        projToInds (Project _ deps) = catMaybes $ intIdFromId <$> deps
        -- Convert the keys to int ids
        mapWithIndsAsDep = projToInds <$> projects
        -- Fold function to create the output map
        foldFct :: Text -> [Int] -> Map Int [Int] -> Map Int [Int]
        foldFct k v m =
            case intIdFromId k of
                Nothing -> m
                Just identifier -> Map.insert identifier v m
    in Map.foldrWithKey foldFct Map.empty mapWithIndsAsDep

-- 

subProject :: Text -> Projects -> Projects
subProject id_ projects = 
    case Map.lookup id_ projects of
        Nothing -> Map.empty
        Just p@(Project _ deps) -> 
            let mapWithOnlyProject = Map.singleton id_ p
                projDeps = catMaybes $ (`Map.lookup` projects) <$> deps
            in Map.union mapWithOnlyProject $ Map.fromList $ zip deps projDeps 


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
    writeFileText (toFilePath file) $ nodes projects <> edges projects

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

