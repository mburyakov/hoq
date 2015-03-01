module REPL
    ( repl
    ) where

import System.Console.Haskeline
import System.Exit
import System.IO
import Control.Monad
import Control.Monad.Trans
import Text.PrettyPrint
import qualified Data.ByteString.Char8 as C
import Data.Bifunctor

import Syntax
import Semantics
import Syntax.Parser
import Syntax.PrettyPrinter
import Syntax.ErrorDoc
import TypeChecking.Monad
import TypeChecking.Expressions
import TypeChecking.Expressions.Utils
import Normalization

ep :: [(Name,Fixity)] -> NF -> String -> ScopeT IO ()
ep tab mode str = do
    mres <- runWarnT $ do
        term <- pExpr tab (C.pack str)
        (term',_) <- typeCheck term Nothing
        return term'
    liftIO $ case mres of
        ([], Just term) -> putStrLn $ render $ ppTerm $ first syntax (nf mode term)
        (errs, _)       -> mapM_ (hPutStrLn stderr . erender . errorMsg) errs

processCmd :: [(Name,Fixity)] -> String -> String -> ScopeT IO ()
processCmd _ "quit" _   = liftIO exitSuccess
processCmd tab "nf"   str = ep tab NF str
processCmd tab "step" str = ep tab Step str
processCmd tab "whnf" str = ep tab WHNF str
processCmd _ cmd _ = liftIO $ hPutStrLn stderr $ "Unknown command " ++ cmd

readLine =
  runInputT defaultSettings . getInputLine

repl :: [(Name,Fixity)] -> ScopeT IO ()
repl tab = do
    mline <- liftIO $ readLine "> "
    case mline of
        Nothing   -> liftIO $ putStrLn ""
        Just line -> case break (== ' ') line of
            ("",_)      -> repl tab
            (c:cmd,line') -> do
                if c == ':' then processCmd tab cmd line' else ep tab NF line
                repl tab
