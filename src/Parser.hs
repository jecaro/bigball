module Parser (parseFile)
    where

import Relude hiding ((<|>), many)

import Text.Parsec
import Text.Parsec.Text

import Project


skipTo :: Parser a -> Parser ()
skipTo p = void $ manyTill anyChar p


skipLine :: Parser ()
skipLine = skipTo endOfLine


projectNameAndId :: Parser (Text, Id)
projectNameAndId = do
    void $ string "Project" >> skipTo (string " = \"")
    name <- toText <$> many1 (alphaNum <|> char '_')
    void $ count 2 $ skipTo (string ", \"")
    identifier <- projectId
    skipLine
    pure (name, identifier)


projectEnd :: Parser ()
projectEnd = string "EndProject" *> skipLine


projectParser :: Parser (Id, Project)
projectParser
    = idAndProject
    <$> projectNameAndId
    <*> option [] section <* projectEnd
  where idAndProject (name, identifier) ids = (identifier, Project name ids)


sectionStart :: Parser ()
sectionStart = spaces >> string "ProjectSection" *> skipLine


sectionEnd :: Parser ()
sectionEnd = spaces >> string "EndProjectSection" *> skipLine


projectId :: Parser Id
projectId = Id . toText <$> between (char '{') (char '}') (many (noneOf ['\n','}']))


sectionLine :: Parser Id
sectionLine = spaces >> projectId <* skipLine


section :: Parser [Id]
section = between sectionStart sectionEnd $ many (try sectionLine)


parseFile :: Parser Projects
parseFile = do
    _ <- count 4 skipLine
    projectsList <- many (try projectParser)
    pure $ fromList projectsList

