{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Char
import Data.Maybe
import Data.List
import qualified Data.Set as Set
import Data.Either
import Control.Applicative hiding (Const)
import Control.Monad hiding (function)
import Control.Monad.Fail

import LLVM.AST hiding (function)
import LLVM.AST.Type hiding (void)
import LLVM.IRBuilder.Instruction       
import LLVM.IRBuilder.Module
import LLVM.IRBuilder.Monad
import LLVM.IRBuilder.Constant
import           LLVM.Linking

import qualified LLVM.CodeGenOpt          as CodeGenOpt
import qualified LLVM.CodeModel           as CodeModel
import qualified LLVM.Relocation          as Reloc
import           Data.IORef
import           Foreign.Ptr
import           LLVM.Target
import           LLVM.Context
import qualified LLVM.Module              as M
import           LLVM.OrcJIT
import           LLVM.OrcJIT.CompileLayer
import           LLVM.Internal.ObjectFile

foreign import ccall "dynamic" mkFun :: FunPtr (IO Int) -> (IO Int)

run :: FunPtr a -> IO Int
run fn = mkFun (castFunPtr fn :: FunPtr (IO Int))


data Token
    = TokSym   Char
    | TokArr
    | TokInt   Int
    | TokIdent String
    deriving (Show, Eq, Ord)


newtype Parser a 
    = Parser { getParser :: ([Token] -> [(a, [Token])]) }

instance Functor Parser where
    fmap f p = Parser $ \toks -> [ (f x, toks') | (x, toks') <- (getParser p) toks ]

instance Applicative Parser where
    pure x    = Parser $ \toks -> [(x, toks)]
    pf <*> px = Parser $ \toks ->
        concat [ (getParser $ fmap f px) toks' | (f, toks') <- (getParser pf) toks ]
 
instance Monad Parser where
    return  = pure
    p >>= f = Parser $ \toks ->
        concat [ (getParser $ f x) toks' | (x, toks') <- (getParser p) toks ]

instance Alternative Parser where
    empty     = Parser $ \toks -> []
    pa <|> pb = Parser $ \toks -> (getParser pa) toks ++ (getParser pb) toks
    
instance MonadFail Parser where
    fail = error


syms = ['+', '-', '*', '/', '\\', '.', '(', ')', '=']


lexToken :: String -> (Token, String) 
lexToken s = case s of
    '-':'>':ss           -> (TokArr, ss)
    c:ss | c `elem` syms -> (TokSym c, ss)
    c:_  | isDigit c     -> (TokInt (read $ takeWhile isDigit s), dropWhile isDigit s)
    c:_  | isAlpha c     -> (TokIdent (takeWhile isAlpha s), dropWhile isAlpha s)
    ' ':ss               -> lexToken ss
    _                    -> error ("lex error at: " ++ s)


lexTokens :: String -> [Token]
lexTokens s = case lexToken s of
    (tok, "") -> [tok]
    (tok, s)  -> (tok:lexTokens s)


-- <closed> ::= '(' <open> ')'
--           |  <var>
--
-- <open> ::= '\' <var>+ '->' <open>
--         |   <closed>+
-- 
-- <expr> ::= <open> | <closed>


data Expr
    = Lam String Expr
    | App Expr Expr
    | Var String
    deriving (Eq, Ord)

instance Show Expr where
    show (Var s)   = s
    show (Lam s e) = "\0955" ++ s ++ "." ++ show e
    show (App m n) = "(" ++ show m ++ " " ++ show n ++ ")"


parseToken :: Token -> Parser Token
parseToken tok = Parser $ \toks -> case toks of
    (t:ts) | t == tok -> [(t, ts)]
    _                 -> []

parseInt :: Parser Int
parseInt = Parser $ \toks -> case toks of
    (TokInt n:ts) -> [(n, ts)]
    _             -> []

parseIdent :: Parser String
parseIdent = Parser $ \toks -> case toks of
    (TokIdent s:ts) -> [(s, ts)]
    _               -> []

parseVar :: Parser Expr
parseVar =
    Var <$> parseIdent

parseLam :: Parser Expr
parseLam = do
    parseToken (TokSym '\\')
    ss <- some parseIdent
    parseToken TokArr
    e <- parseOpen
    return (foldr Lam e ss)

parseApp :: Parser Expr
parseApp = do
    foldl1 App <$> some parseClosed

parseOpen :: Parser Expr
parseOpen = parseLam <|> parseApp

parseClosed :: Parser Expr
parseClosed = parseVar <|> do
    parseToken (TokSym '(')
    e <- parseOpen
    parseToken (TokSym ')')
    return e

parseExpr :: Parser Expr
parseExpr = parseOpen
    

parseAssign :: Parser (String, Expr)
parseAssign = do
    id <- parseIdent
    parseToken (TokSym '=')
    e <- parseExpr
    return (id, e)


data Line
    = Assign String Expr
    | Eval Expr
    deriving (Show, Eq, Ord)


parseLine :: Parser Line
parseLine = (Eval <$> parseExpr) <|> ((\(s, e) -> Assign s e) <$> parseAssign)

parse :: [Token] -> Line
parse toks = case (Set.toList $ Set.fromList $ filter (null . snd) (getParser parseLine $ toks)) of
    []        -> error "can't parse"
    [(l, [])] -> l
    x         -> error ("more than one parse" ++ show x)


freeVars :: [String] -> Expr -> [String]
freeVars vs (Var s)   = if s `elem` vs then [] else [s]
freeVars vs (App a b) = union (freeVars vs a) (freeVars vs b)
freeVars vs (Lam s e) = freeVars (s:vs) e
--
--
--combinate :: [(String, Expr)] -> Expr -> Expr
--combinate env (Lam s e) = case combinate env e of
--    Var s' | s' == s                    -> App (App (Var "s") (Var "k")) (Var "k")
--    e'     | s `notElem` freeVars [] e' -> App (Var "k") e'
--    App m n                             -> App (App (Var "s") (combinate env $ Lam s m)) (combinate env $ Lam s n)
--combinate env (Var s) = case lookup s env of
--    Just t  -> combinate env t
--    Nothing -> Var s
--combinate env (App m n) = App (combinate env m) (combinate env n)
--

eval :: [(String, Expr)] -> Expr -> Expr
eval env (Var s) | Just e <- lookup s env  = eval env e
eval env (Var s)                           = Var s
eval env (Lam s e)                         = Lam s e
eval env (App (Lam s e) n)                 = eval (insert (s, n) env) e
eval env (App m n)                         = eval env $ App (eval env m) (eval env n)

main :: IO ()
main = repl []
    where
        repl :: [(String, Expr)] -> IO ()
        repl env = do
            line <- getLine
            case line of
                'q':_ -> return ()
                _     -> do
                    case parse (lexTokens line) of
                        Assign id e -> repl $ insert (id, e) env
                        Eval e      -> do
                            putStrLn $ "eval:     " ++ show (eval env e)
                            repl env
            


