module YulParser(parseYul) where
{-
import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Void
-}
import LightYear
import Text.Megaparsec.Char.Lexer qualified as L
import Yul


parseYul :: String -> Yul
parseYul = runMyParser yulProgram

sc :: Parser ()
sc = L.space space1 
             (L.skipLineComment "//") 
             (L.skipBlockComment "/*" "*/")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser String
symbol = L.symbol sc

identifier :: Parser String
identifier = lexeme ((:) <$> letterChar <*> many alphaNumChar)

integer :: Parser Integer
integer = lexeme L.decimal

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

commaSep :: Parser a -> Parser [a]
commaSep p = p `sepBy` symbol ","

yulExpression :: Parser YulExpression
yulExpression = choice
    [ (YulLiteral . YulNumber) <$> integer
    , YulIdentifier <$> identifier
    , YulCall <$> identifier <*> parens (commaSep yulExpression)
    ]

yulStatement :: Parser YulStatement
yulStatement = choice
    [ YulBlock <$> between (symbol "{") (symbol "}") (many yulStatement)
    , yulFun 
    , YulLet <$> (symbol "let" *> commaSep identifier) <*> optional (symbol ":=" *> yulExpression)
    , YulAssign <$> commaSep identifier <*> (symbol ":=" *> yulExpression)
    ]

yulFun :: Parser YulStatement
yulFun = do
    _ <- symbol "function"
    name <- identifier
    args <- parens (commaSep identifier)
    rets <- optional (symbol "->" *> (commaSep identifier))
    stmts <- between (symbol "{") (symbol "}") (many yulStatement)
    return (YulFun name args rets stmts)

yulProgram :: Parser Yul
yulProgram = sc *> (Yul <$> many yulStatement) <* eof