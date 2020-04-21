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

