-- | Applicative parser for a .sln file
module Parser (graph, render) where

import Graph (Graph, fromProjects)
import Project (Id (..), Project (..))
import Relude hiding (many, (<|>))
import Text.Parsec
  ( ParseError,
    alphaNum,
    anyChar,
    between,
    char,
    count,
    endOfLine,
    many,
    many1,
    manyTill,
    noneOf,
    notFollowedBy,
    option,
    skipMany,
    spaces,
    string,
    try,
    (<|>),
  )
import Text.Parsec.Text (Parser)

-- | Skip characters until a specific character is found
skipTo :: Parser a -> Parser ()
skipTo p = void $ manyTill anyChar p

-- | Skip the rest of the line
skipLine :: Parser ()
skipLine = skipTo endOfLine

-- | Parse the project name and its id
projectNameAndId :: Parser (Text, Id)
projectNameAndId = do
  projectStartOfLine >> skipTo (string " = \"")
  name <- toText <$> many1 (alphaNum <|> char '_')
  _ <- count 2 $ skipTo (string ", \"")
  identifier <- projectId
  skipLine
  pure (name, identifier)

-- | Parse the start of a 'Project' line
projectStartOfLine :: Parser ()
projectStartOfLine = void $ string "Project"

-- | Skip project line
projectEnd :: Parser ()
projectEnd = string "EndProject" *> skipLine

-- | Parser for the project
projectParser :: Parser Project
projectParser =
  idAndProject
    <$> projectNameAndId
    <*> option [] section
    <* projectEnd
  where
    idAndProject (name, identifier) = Project identifier name

-- | Parse the beginning of a section
sectionStart :: Parser ()
sectionStart = spaces >> string "ProjectSection" *> skipLine

-- | Parse the end of a section
sectionEnd :: Parser ()
sectionEnd = spaces >> string "EndProjectSection" *> skipLine

-- | Parse the project 'Id'
projectId :: Parser Id
projectId = Id . toText <$> between (char '{') (char '}') (many (noneOf ['\n', '}']))

-- | Parse a dependency
sectionLine :: Parser Id
sectionLine = spaces >> projectId <* skipLine

-- | Parse all the dependencies of a project
section :: Parser [Id]
section = between sectionStart sectionEnd $ many (try sectionLine)

-- | Parse the sln file and build up the graph with the project list
graph :: Parser Graph
graph = do
  skipMany (notFollowedBy projectStartOfLine >> skipLine)
  projectsList <- many projectParser
  pure $ fromProjects projectsList

-- | Output a readable error message
render :: ParseError -> Text
render msg = "Parse error: " <> show msg
