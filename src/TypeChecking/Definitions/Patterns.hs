{-# LANGUAGE ExistentialQuantification #-}

module TypeChecking.Definitions.Patterns
    ( typeCheckPatterns
    , TermsInCtx(..)
    ) where

import Syntax.Expr as E
import Syntax.Term as T
import Syntax.ErrorDoc
import Syntax.Scope
import TypeChecking.Context
import TypeChecking.Monad
import TypeChecking.Definitions.Coverage as C
import TypeChecking.Expressions
import Normalization

data TermInCtx b  = forall a. Eq a => TermInCtx  (Ctx String Term b a) (Term a)
data TermsInCtx b = forall a. Eq a => TermsInCtx (Ctx String Term b a) [Term a] (Term a)

typeCheckPattern :: (Monad m, Eq a) => Ctx String Term String a
    -> Term a -> ParPat -> TCM m (Bool, Maybe (TermInCtx a), T.Pattern, C.Pattern)
typeCheckPattern ctx T.Interval (ParLeft _) =
    return (False, Just $ TermInCtx Nil $ ICon ILeft , T.PatternI ILeft , PatLeft)
typeCheckPattern ctx T.Interval (ParRight _) =
    return (False, Just $ TermInCtx Nil $ ICon IRight, T.PatternI IRight, PatRight)
typeCheckPattern ctx ty@(DataType _ 0 _) (ParEmpty _) = return (True, Nothing, T.PatternAny, PatVar)
typeCheckPattern ctx ty (ParEmpty (PPar (lc,_))) =
    throwError [emsgLC lc "" $ pretty "Expected non-empty type: " <+> prettyOpen ctx ty]
typeCheckPattern ctx ty (ParVar (NoArg _)) = return (False, Nothing, T.PatternAny, PatVar)
typeCheckPattern ctx ty@(DataType dt _ params) (ParVar (Arg (PIdent (lc,var)))) = do
    cons <- lift $ getConstructor var (Just dt)
    case cons of
        [] -> return (False, Just $ TermInCtx (Snoc Nil var ty) $ T.Var Bound, T.PatternVar, PatVar)
        (n,con,(conType,_)):_ -> if isDataType conType
            then let con'@(T.Con i _ _ _) = instantiate params $ fmap (liftBase ctx) con
                 in return (False, Just $ TermInCtx Nil con', T.Pattern i [], C.Pattern i n [])
            else throwError [emsgLC lc ("Not enough arguments to " ++ show var) enull]
  where
    isDataType :: Scope a Term b     -> Bool
    isDataType (ScopeTerm DataType{}) = True
    isDataType (ScopeTerm _)          = False
    isDataType (Scope _ t)            = isDataType t
typeCheckPattern ctx ty (ParVar (Arg (PIdent (lc,var)))) =
    return (False, Just $ TermInCtx (Snoc Nil var ty) $ T.Var Bound, T.PatternVar, PatVar)
typeCheckPattern ctx dty@(DataType dt _ params) (ParPat _ (E.Pattern (PIdent (lc,conName)) pats)) = do
    cons <- lift $ getConstructor conName (Just dt)
    case cons of
        []        -> throwError [notInScope lc "data constructor" conName]
        (n,con,(conType,_)):_ -> do
            let T.Con i _ conds _ = instantiate params $ fmap (liftBase ctx) con
            let conType'          = instantiate params $ fmap (liftBase ctx) conType
            (bf, TermsInCtx ctx' terms ty, rtpats, cpats) <- typeCheckPatterns ctx conType' pats
            let res = TermInCtx ctx' (T.Con i conName conds terms)
            case nf WHNF ty of
                DataType{} -> return (bf, Just res, T.Pattern i rtpats, C.Pattern i n cpats)
                _          -> throwError [emsgLC lc "Not enough arguments" enull]
typeCheckPattern ctx ty pat =
    throwError [emsgLC (parPatGetPos pat) "" $ pretty "Unexpected pattern" $$
                                               pretty "Expected type:" <+> prettyOpen ctx ty]

typeCheckPatterns :: (Monad m, Eq a) => Ctx String Term String a -> Term a -> [ParPat]
    -> TCM m (Bool, TermsInCtx a, [T.Pattern], [C.Pattern])
typeCheckPatterns _ ty [] = return (False, TermsInCtx Nil [] ty, [], [])
typeCheckPatterns ctx (T.Pi a b) (pat:pats) = do
    (bf1, mte, rtpat, cpat) <- typeCheckPattern ctx (nf WHNF a) pat
    TermInCtx ctx' te <- case mte of
                            Nothing -> return $ TermInCtx Nil $ T.Var $ liftBase ctx $
                                case b of Scope v _ -> v
                                          _         -> "_"
                            Just te -> return te
    let b' = instantiate1 te $ fmap (liftBase $ liftCtx ctx') (dropOnePi a b)
    (bf2, TermsInCtx ctx'' tes ty, rtpats, cpats) <- typeCheckPatterns (ctx +++ ctx') (nf WHNF b') pats
    return (bf1 || bf2, TermsInCtx (ctx' +++ ctx'') (fmap (liftBase ctx'') te : tes) ty, rtpat:rtpats, cpat:cpats)
typeCheckPatterns _ _ (pat:_) = throwError [emsgLC (parPatGetPos pat) "Too many arguments" enull]
