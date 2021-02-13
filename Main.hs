{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Char
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


data Operator
    = Plus
    | Minus
    | Times
    | Divide
    deriving (Show, Eq)


data Token
    = TokSym   Char
    | TokInt   Int
    | TokIdent String
    | TokEnd
    deriving (Show, Eq)


data Expr
    = Lamb  Expr Expr
    | App   Expr Expr
    | Const Int
    | Ident String
    deriving (Show, Eq)


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


syms = ['+', '-', '*', '/', '\\', '.']


lexToken :: String -> (Token, String) 
lexToken s = case s of
    c:ss | c `elem` syms -> (TokSym c, ss)
    c:_  | isDigit c     -> (TokInt (read $ takeWhile isDigit s), dropWhile isDigit s)
    c:_  | isAlpha c     -> (TokIdent (takeWhile isAlpha s), dropWhile isAlpha s)
    ""                   -> (TokEnd, "")
    ' ':ss               -> lexToken ss
    _                    -> error ("lex error at: " ++ s)


lexTokens :: String -> [Token]
lexTokens s = case lexToken s of
    (tok, "") -> [tok]
    (tok, s)  -> (tok:lexTokens s)


parseToken :: Token -> Parser Token
parseToken tok = Parser $ \toks -> case toks of
    (t:ts) | t == tok -> [(t, ts)]
    _                 -> []



parseInt :: Parser Int
parseInt = Parser $ \toks -> case toks of
    (TokInt n:ts) -> [(n, ts)]
    _             -> []

parseIdent :: Parser Expr
parseIdent = Parser $ \toks -> case toks of
    (TokIdent s:ts) -> [(Ident s, ts)]
    _               -> []


-- <expr> ::= <const>
--         | <ident>
--         | '\' <ident> '.' <expr>
--         | '(' <expr> <expr> ')'
--         | '(' '+' <expr> ')'
--         | '(' '-' <expr> ')'
--         | '(' '*' <expr> ')'
--         | '(' '/' <expr> ')'

parseConst :: Parser Expr
parseConst =
    Const <$> parseInt


parseApp :: Parser Expr
parseApp = do
    parseToken (TokSym '(')
    a <- parseExpr
    b <- parseExpr
    parseToken (TokSym ')')
    return (App a b)

parseLamb :: Parser Expr
parseLamb = do
    parseToken (TokSym '\\')
    id <- parseIdent
    parseToken (TokSym '.')
    Lamb id <$> parseExpr

parseExpr :: Parser Expr
parseExpr =
    parseConst <|>
    parseIdent <|>
    parseLamb  <|>
    parseApp <|>


parse :: [Token] -> Expr
parse toks = case filter (null . snd) (getParser parseExpr $ toks) of
    []        -> error "can't parse"
    [(e, [])] -> e
    _         -> error "more than one parse"


compile :: Expr -> ModuleBuilder ()
compile expr =
    void $ function (mkName "main") [] i64 $ \_ ->
        ret =<< compileExpr expr


compileExpr :: Monad m => Expr -> IRBuilderT m Operand
compileExpr e = case e of
    Const n         -> return $ int64 (fromIntegral n)


withMyHostTargetMachine :: (TargetMachine -> IO ()) -> IO ()
withMyHostTargetMachine f = do
    initializeNativeTarget
    triple <- getProcessTargetTriple
    cpu <- getHostCPUName
    features <- getHostCPUFeatures
    (target, _) <- lookupTarget Nothing triple
    withTargetOptions $ \options ->
        withTargetMachine target triple cpu features options reloc model genOpt f
    where
        reloc  = Reloc.PIC
        model  = CodeModel.Default
        genOpt = CodeGenOpt.None


withSession :: (Context -> ExecutionSession -> IRCompileLayer ObjectLinkingLayer -> IO ()) -> IO ()
withSession f = do
    resolvers <- newIORef []
    withContext $ \ctx -> 
        withExecutionSession $ \es ->
            withMyHostTargetMachine $ \tm -> 
                withObjectLinkingLayer es (\_ -> head <$> readIORef resolvers) $ \oll ->
                    withIRCompileLayer oll tm $ \cl ->
                        withSymbolResolver es (myResolver cl) $ \psr -> do
                            writeIORef resolvers [psr]
                            f ctx es cl
                                
    where
        myResolver :: IRCompileLayer ObjectLinkingLayer -> SymbolResolver
        myResolver cl = SymbolResolver $ \mangled -> do
            symbol <- findSymbol cl mangled False
            when (isLeft symbol) (error "symbol resolver error")
            return symbol


jitAndRunMain :: [Definition] -> ExecutionSession -> Context -> IRCompileLayer ObjectLinkingLayer -> IO Int
jitAndRunMain defs es ctx cl = do
    let astmod = defaultModule { moduleDefinitions = defs }
    withModuleKey es $ \modKey ->
        M.withModuleFromAST ctx astmod $ \mod -> do
            addModule cl modKey mod
            mangled <- mangleSymbol cl "main"
            Right (JITSymbol fn _) <- findSymbolIn cl modKey mangled False
            r <- run $ castPtrToFunPtr (wordPtrToPtr fn)
            removeModule cl modKey
            return r


main :: IO ()
main =
    withSession $ \ctx es cl -> do
        line <- getLine
        let expr = parse (lexTokens line)
        let defs = execModuleBuilder emptyModuleBuilder (compile expr)
        n <- jitAndRunMain defs es ctx cl
        putStrLn (show n)


