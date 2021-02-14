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


syms = ['+', '-', '*', '/', '\\', '.', '(', ')']


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


-- <expr> ::= <var>
--         |  '\' <var>+ '->' <expr>
--         |  '(' <expr> <expr> ')'

data Expr
    = Lam Expr Expr
    | App Expr Expr
    | Var String
    deriving (Eq, Ord)

instance Show Expr where
    show (Var s)         = s
    show (Lam (Var s) e) = "(\\" ++ s ++ "." ++ show e ++ ")"
    show (App m n)       = show m ++ " " ++ show n


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
    vs <- some parseVar
    parseToken TokArr
    e <- parseExpr
    return $ foldr1 Lam (vs ++ [e])


parseApp :: Parser Expr
parseApp = do
    parseToken (TokSym '(')
    a <- parseExpr
    b <- parseExpr
    parseToken (TokSym ')')
    return (App a b)


parseExpr :: Parser Expr
parseExpr = parseVar <|> parseLam <|> parseApp
    

parse :: [Token] -> Expr
parse toks = case (Set.toList $ Set.fromList $ filter (null . snd) (getParser parseExpr $ toks)) of
    []        -> error "can't parse"
    [(e, [])] -> e
    x         -> error ("more than one parse" ++ show x)

fv :: [String] -> Expr -> [String]
fv vs (Var s)
    | s `elem` vs = []
    | otherwise   = [s]
fv vs (App a b)    = union (fv vs a) (fv vs b)
fv vs (Lam (Var s) e)    = fv (s:vs) e


babs env (Lam x e)
    | y@(Var _) <- t, x == y         = App (App (Var "s") (Var "k")) (Var "k")
    | Var a <- x, a`notElem` fv [] t = App (Var "k") t
    | App m n <- t                   = App (App (Var "s") (babs env $ Lam x m)) (babs env $ Lam x n)
    where t = babs env e
babs env (Var s)
    | Just t <- lookup s env = babs env t
    | otherwise              = Var s
babs env (App m n)           = App (babs env m) (babs env n)

main :: IO ()
main = do
    line <- getLine
    let expr = parse (lexTokens line)
    putStrLn (show expr)
    putStrLn (show $ babs [] expr)
    main


