{-# LANGUAGE ApplicativeDo #-}

module Parse (findImports) where

import           Data.Char              (isSpace)
import           Data.Function          ((&))
import           Data.Maybe             (fromMaybe, mapMaybe)
import           Text.Regex.Applicative

data Lexeme
    = SingleLineComment String
    | MultiLineComment String
    | ImportSideEffect String
    | Other
    deriving Show

singleQuote = '\''
doubleQuote = '"'

manyNot :: Char -> RE Char String
manyNot ch = many $ psym (/= ch)

singleLineComment :: RE Char Lexeme
singleLineComment = do
    string "//"
    comment <- manyNot '\n'
    return $ SingleLineComment comment

multiLineComment :: RE Char Lexeme
multiLineComment = do
    string "/*"
    comment <- few anySym <* string "*/"
    return $ MultiLineComment comment

importSideEffect :: RE Char Lexeme
importSideEffect = do
    string "import"
    someWhitespace
    module_ <- bracket singleQuote <|> bracket doubleQuote
    return $ ImportSideEffect module_

bracket :: Char -> RE Char String
bracket ch = sym ch *> manyNot ch <* sym ch

someWhitespace :: RE Char String
someWhitespace = some $ psym isSpace

other :: RE Char Lexeme
other = do
    anySym
    return Other

lexeme :: RE Char [Lexeme]
lexeme = many $ (singleLineComment <|> multiLineComment <|> importSideEffect <|> other)

extractImport :: Lexeme -> Maybe String
extractImport (ImportSideEffect i) = Just i
extractImport _                    = Nothing

findImports :: String -> [String]
findImports contents =
      contents =~ lexeme
    & fromMaybe []
    & mapMaybe extractImport

