import Text.Parsec
import Text.Parsec.String (Parser)

-- Define the AST for mathematical expressions
data Expr
  = Lit Double
  | Add Expr Expr
  | Sub Expr Expr
  | Mul Expr Expr
  | Div Expr Expr
  deriving (Show)

-- | Parses a floating-point number.
double :: Parser Double
double = do
  intPart <- many1 digit
  fracPart <- option "" $ do
    _ <- char '.'
    fracPart <- many1 digit
    return ('.' : fracPart)
  expPart <- option "" $ do
    _ <- oneOf "eE"
    sign <- option "" $ string "+" <|> string "-"
    exponent <- many1 digit
    return ('e' : sign ++ exponent)
  return $ read (intPart ++ fracPart ++ expPart)

-- | Parses a parenthesized expression.
parens :: Parser a -> Parser a
parens p = char '(' *> p <* char ')'

-- | Parses a mathematical expression.
expr :: Parser Expr
expr = try term >>= addOrSub
  where
    term = try (parens expr) <|> lit
    lit = Lit <$> double
    addOrSub left = do
      op <- choice [char '+' >> return Add, char '-' >> return Sub]
      right <- term
      addOrSub (op left right)
        <|> return left


-- | Evaluates a mathematical expression.
eval :: Expr -> Double
eval (Lit x) = x
eval (Add e1 e2) = eval e1 + eval e2
eval (Sub e1 e2) = eval e1 - eval e2
eval (Mul e1 e2) = eval e1 * eval e2
eval (Div e1 e2) = eval e1 / eval e2


-- Define a main function that reads user input, evaluates it, and prints the result
main :: IO ()
main = do
  putStrLn "Enter a mathematical expression:"
  input <- getLine
  case parse expr "" input of
    Left err -> putStrLn ("Error: " ++ show err)
    Right expr -> putStrLn ("Result: " ++ show (eval expr))
