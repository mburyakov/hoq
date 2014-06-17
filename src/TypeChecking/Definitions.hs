module TypeChecking.Definitions
    ( PDef(..), Tele
    , splitDefs
    , parPatToPattern, toListPat
    ) where

import Control.Monad

import Syntax.Expr
import qualified Syntax.Term as T
import TypeChecking.Expressions
import TypeChecking.Monad

exprPatToPattern :: Monad m => Pattern -> TCM m T.RTPattern
exprPatToPattern (Pattern (PIdent (lc,name)) pats) = do
    d <- lift (getEntry name)
    case d of
        Just (T.Con i _ _, _) -> liftM (T.RTPattern i) (mapM parPatToPattern pats)
        _ -> throwError [notInScope lc "data constructor" name]

parPatToPattern :: Monad m => ParPat -> TCM m T.RTPattern
parPatToPattern (ParVar arg) = do
    d <- lift $ getEntry (unArg arg)
    return $ case d of
        Just (T.Con i _ _, _) -> T.RTPattern i []
        _ -> T.RTPatternVar
parPatToPattern (ParPat _ pat) = exprPatToPattern pat

toListPat :: T.RTPattern -> Pattern -> [String]
toListPat T.RTPatternVar (Pattern (PIdent (_,name)) _) = [name]
toListPat (T.RTPattern _ pats) (Pattern _ pats') = concat (zipWith toListPar pats pats')
  where
    toListPar :: T.RTPattern -> ParPat -> [String]
    toListPar pat (ParPat _ pat') = toListPat pat pat'
    toListPar T.RTPatternVar (ParVar arg) = [unArg arg]
    toListPar (T.RTPattern _ _) (ParVar _) = []

type Tele = [([Arg], Expr)]
data PDef = PDefSyn String Expr | PDefCases String Expr [([ParPat],Expr)] | PDefData String Tele [(String,Tele)]

theSameAs :: String -> Def -> Bool
theSameAs name (DefFun (Pattern (PIdent (_,name')) _) _) = name == name'
theSameAs _ _ = False

splitDefs :: Monad m => [Def] -> EDocM m [PDef]
splitDefs [] = return []
splitDefs (DefType (PIdent (_,name)) ty : defs) =
    let (defs1,defs2) = span (theSameAs name) defs
    in liftM (PDefCases name ty (map (\(DefFun (Pattern _ pats) expr) -> (pats,expr)) defs1) :) (splitDefs defs2)
splitDefs (DefFun (Pattern (PIdent (_,name)) []) expr : defs) = liftM (PDefSyn name expr :) (splitDefs defs)
splitDefs (DefFun (Pattern (PIdent (lc,name)) _) _    : defs) = do
    warn [inferErrorMsg lc]
    splitDefs $ dropWhile (theSameAs name) defs
splitDefs (DefData (PIdent (_,name)) teles cons : defs) = do
    dataTeles <- forM teles $ \(DataTele _ e1 e2) -> liftM (\vs -> (vs, e2)) (exprToVars e1)
    conTeles  <- forM (unCons cons) $ \(Con (PIdent (_,con)) teles) -> do
        teles' <- forM teles $ \tele ->
            case tele of
                VarTele _ e1 e2 -> liftM (\vs -> (vs, e2)) (exprToVars e1)
                TypeTele e2     -> return ([], e2)
        return (con,teles')
    pdefs <- splitDefs defs
    return (PDefData name dataTeles conTeles : pdefs)
