module REPL
    ( repl
    ) where

import System.Console.Readline
import System.Exit
import System.IO
import Control.Monad
import Control.Monad.Trans
import Data.Traversable(sequenceA)
import Text.PrettyPrint

import Syntax.BNFC.ParGrammar
import Syntax.BNFC.ErrM
import qualified Syntax.Expr as E
import Syntax.Term
import TypeChecking
import Evaluation.Monad
import Evaluation.Normalization

parseExpr :: Monad m => String -> EvalT String RTDef Term m (Either String (Term (String, Maybe (Ref String RTDef Term))))
parseExpr s = case parser s of
    Bad err -> return (Left err)
    Ok expr -> case sequenceA (typeCheck expr) of
        Left err -> return (Left err)
        Right term -> liftM Right (substInTerm term)
  where
    parser :: String -> Err E.Expr
    parser = pExpr . myLexer

processCmd :: String -> String -> EvalT String RTDef Term IO ()
processCmd "quit" _ = liftIO exitSuccess
processCmd cmd str | Just mode <- nfMode cmd = do
    res <- parseExpr str
    liftIO $ case res of
        Left err -> hPutStrLn stderr err
        Right term -> putStrLn $ render $ ppTerm $ fmap fst (nf mode term)
  where
    nfMode "whnf" = Just WHNF
    nfMode "wnf"  = Just WNF
    nfMode "nf"   = Just NF
    nfMode _      = Nothing
processCmd cmd _ = liftIO $ hPutStrLn stderr $ "Unknown command " ++ cmd

repl :: EvalT String RTDef Term IO ()
repl = go ""
  where
    go last = do
        mline <- liftIO $ readline "> "
        case mline of
            Nothing   -> liftIO $ putStrLn ""
            Just line -> case break (== ' ') line of
                ("",_)      -> go last
                (cmd,line') -> do
                    when (line /= last) $ liftIO (addHistory line)
                    processCmd cmd line'
                    go line