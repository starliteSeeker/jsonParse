module Json where

import qualified Data.HashMap.Lazy as M
import Data.Scientific (Scientific)
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

cool :: Int
cool = 3

data Value
  = VStr String
  | VNum Scientific
  | VObj Object
  | VArr Array
  | VTrue
  | VFalse
  | VNull
  deriving (Show)

type Object = M.HashMap String Value

type Array = [Value]

type Parser = Parsec Void String

-- file_name -> contents -> result
parseJson :: String -> String -> Either (ParseErrorBundle String Void) Value
parseJson = parse $ between skipSpace eof json

-- https://serokell.io/blog/parser-combinators-in-haskell
skipSpace :: Parser ()
skipSpace = L.space space1 empty empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme skipSpace

value :: Parser Value
value = lexeme $ choice [tfn, VStr <$> str, sci, arr, obj]

json :: Parser Value
json = lexeme $ choice [arr, obj]

tfn :: Parser Value
tfn =
  label "boolean" (VTrue <$ string "true" <|> VFalse <$ string "false")
    <|> VNull <$ string "null"

-- escaping not implemented
str :: Parser String
str = label "string" (char '"' >> manyTill L.charLiteral (char '"'))

sci :: Parser Value
sci = VNum <$> L.signed (pure ()) L.scientific

comma :: Parser Char
comma = lexeme $ char ','

arr :: Parser Value
arr = between (lexeme $ char '[') (char ']') $ VArr <$> sepBy value comma

obj :: Parser Value
obj = between (lexeme $ char '{') (char '}') $ VObj . M.fromList <$> sepBy pair comma
  where
    pair = (,) <$> (lexeme str <* lexeme (char ':')) <*> value
