{-# LANGUAGE FlexibleContexts, ExistentialQuantification #-}

module TypeChecking.Expressions
    ( typeCheck, typeCheckCtx
    , notInScope, inferErrorMsg, inferParamsErrorMsg
    , prettyOpen, exprToVars
    , checkUniverses
    ) where

import Control.Monad
import Data.List
import Data.Maybe
import Data.Traversable

import Syntax.Expr as E
import Syntax.Term as T
import Syntax.ErrorDoc
import TypeChecking.Monad
import TypeChecking.Context
import Normalization

notInScope :: Show a => (Int,Int) -> String -> a -> EMsg f
notInScope lc s a = emsgLC lc ("Not in scope: " ++ (if null s then "" else s ++ " ") ++ show a) enull

inferErrorMsg :: (Int,Int) -> String -> EMsg f
inferErrorMsg lc s = emsgLC lc ("Cannot infer type of " ++ s) enull

inferParamsErrorMsg :: Show a => (Int,Int) -> a -> EMsg f
inferParamsErrorMsg lc d = emsgLC lc ("Cannot infer parameters of data constructor " ++ show d) enull

prettyOpen :: (Pretty b f, Monad f) => Ctx b g b a -> f a -> EDoc f
prettyOpen ctx term = epretty $ liftM pretty (close ctx term)

exprToVars :: Monad m => Expr -> EDocM m [Arg]
exprToVars = liftM reverse . go
  where
    go (E.Var a) = return [a]
    go (E.App as (E.Var a)) = liftM (a:) (go as)
    go e = throwError [emsgLC (getPos e) "Expected a list of identifiers" enull]

checkUniverses :: (Pretty b Term, Monad m) => Ctx b g b a1 -> Ctx b g b a2
    -> Expr -> Expr -> Term a1 -> Term a2 -> EDocM m Level
checkUniverses _ _ _ _ (T.Universe u1) (T.Universe u2) = return (max u1 u2)
checkUniverses ctx1 ctx2 e1 e2 t1 t2 = throwError $ checkIsType ctx1 e1 t1 ++ checkIsType ctx2 e2 t2

checkIsType :: Pretty b Term => Ctx b g b a -> Expr -> Term a -> [EMsg Term]
checkIsType _ _ (T.Universe _) = []
checkIsType ctx e t = [emsgLC (getPos e) "" $ pretty "Expected type: Type" $$
                                              pretty "Actual type:" <+> prettyOpen ctx t]

data SomeEq f = forall a. Eq a => SomeEq (f a)

typeCheck :: Monad m => Expr -> Maybe (Type String) -> TCM m (Term String, Type String)
typeCheck = typeCheckCtx Nil

typeCheckCtx :: (Monad m, Eq a) => Ctx String Type String a -> Expr -> Maybe (Type a) -> TCM m (Term a, Type a)
typeCheckCtx ctx expr ty = go ctx expr [] $ fmap (\(Type term lvl) -> Type (nf WHNF term) lvl) ty
  where
    go :: (Monad m, Eq a) => Ctx String Type String a -> Expr -> [Expr] -> Maybe (Type a) -> TCM m (Term a, Type a)
    go ctx (Paren _ e) exprs ty = go ctx e exprs ty
    go ctx (E.App e1 e2) exprs ty = go ctx e1 (e2:exprs) ty
    go ctx (E.Lam _ [] e) exprs ty = go ctx e exprs ty
    go ctx (E.Lam p (arg : args) e) [] (Just (Type ty lvl)) = case ty of
        T.Pi a b -> do
            let var = unArg arg
            (te, _) <- go (Snoc ctx var $ Type a lvl) (E.Lam p args e) [] $ Just $ Type (nf WHNF $ dropOnePi a b) lvl
            return (T.Lam $ Scope1 var te, Type ty lvl)
        _ -> throwError [emsgLC (argGetPos arg) "" $ pretty "Expected type:" <+> prettyOpen ctx ty $$
                                                     pretty "But lambda expression has pi type"]
    go _ e@E.Lam{} _ _ = throwError [inferErrorMsg (getPos e) "the argument"]
    go ctx (E.Var (NoArg (Pus (lc,_)))) exprs mty = throwError [emsgLC lc "Expected an identifier" enull]
    go ctx (E.Var (Arg (PIdent (lc,var)))) exprs mty = do
        (te, Type ty lvl) <- case lookupCtx var ctx of
            Just r  -> return r
            Nothing -> do
                mt <- lift $ getEntry var $ case mty of
                    Just (Type (DataType d _ _) _) -> Just d
                    _                              -> Nothing
                case mt of
                    [FunctionE te ty] -> return (fmap (liftBase ctx) te , fmap (liftBase ctx) ty)
                    [DataTypeE ty e]  -> return (DataType var e []      , fmap (liftBase ctx) ty)
                    [ConstructorE _ con (ty, lvl)] -> case mty of
                        Just (Type (DataType _ _ params) _) ->
                            let liftTerm = instantiate params . fmap (liftBase ctx)
                            in  return (liftTerm con, Type (liftTerm ty) lvl)
                        Just (Type ty _) -> throwError [emsgLC lc "" $ pretty "Expected type:" <+> prettyOpen ctx ty $$
                                                                       pretty ("But given data constructor " ++ show var)]
                        Nothing -> throwError [inferParamsErrorMsg lc var]
                    [] -> do
                        cons <- lift (getConstructorDataTypes var)
                        let Type (DataType dataType _ _) _ = fromJust mty
                        case cons of
                            []    -> throwError [notInScope lc "" var]
                            [act] -> throwError [emsgLC lc "" $
                                pretty ("Expected data type: " ++ dataType) $$
                                pretty ("Actual data type: " ++ act)]
                            acts -> throwError [emsgLC lc "" $
                                pretty ("Expected data type: " ++ dataType) $$
                                pretty ("Posible data types: " ++ intercalate ", " acts)]
                    _  -> throwError [inferErrorMsg lc $ show var]
        (tes, Type ty' lvl') <- typeCheckApps lc ctx exprs (Type ty lvl)
        case (mty, ty') of
            (Nothing, _)  -> return ()
            (Just (Type (DataType edt _ _) _), DataType adt _ []) -> unless (edt == adt) $
                throwError [emsgLC lc "" $ pretty ("Expected data type: " ++ edt) $$
                                           pretty ("Actual data type: " ++ adt)]
            (Just (Type ety _), _) -> actExpType ctx ty' ety lc
        return (apps te tes, Type ty' $ maybe lvl' (\(Type _ lvl'') -> min lvl' lvl'') mty)
    go _ (ELeft _)  [] Nothing = return (ICon ILeft, Type T.Interval NoLevel)
    go _ e@ELeft{}  _  Nothing = throwError [emsgLC (getPos e) "\"left\" is applied to arguments" enull]
    go _ (ERight _) [] Nothing = return (ICon IRight, Type T.Interval NoLevel)
    go _ e@ERight{} _  Nothing = throwError [emsgLC (getPos e) "\"right\" is applied to arguments" enull]
    go ctx e@PathCon{} es _ | length es > 1 = throwError [emsgLC (getPos e) "A path is applied to arguments" enull]
    go ctx e@PathCon{} [] Nothing = throwError [inferErrorMsg (getPos e) "path"]
    go ctx PathCon{} [e] Nothing = throwError [emsgLC (getPos e) "Not implemented yet" enull] -- TODO
{-
        (r,t) <- go ctx e [] Nothing
        case t of
            T.Arr T.Interval t'  -> ...
            T.Pi _ T.Interval t' -> ...
            _                    -> ...
-}
    go ctx e@PathCon{} [] (Just ty) = throwError [emsgLC (getPos e) "Not implemented yet" enull] -- TODO
    go ctx e'@PathCon{} [e] (Just (Type ty@(T.Path h mt1 _) lvl)) = do
        (r,t) <- go ctx e [] $ fmap (\t1 -> Type (T.Pi T.Interval $
            Scope "i" $ ScopeTerm $ T.App (fmap Free t1) $ T.Var Bound) lvl) mt1
        let left  = T.App r (ICon ILeft)
            right = T.App r (ICon IRight)
        actExpType ctx (T.Path Implicit Nothing [left,right]) ty (getPos e')
        return (PCon (Just r), Type ty lvl)
    go ctx e'@PathCon{} [e] (Just (Type ty _)) =
        throwError [emsgLC (getPos e') "" $ pretty "Expected type:" <+> prettyOpen ctx ty $$
                                            pretty "Actual type: Path"]
    go ctx (E.At e1 e2) es Nothing = do
        (r1, Type t1 lvl) <- go ctx e1 [] Nothing
        (r2, _) <- go ctx e2 [] $ Just (Type T.Interval NoLevel)
        case nf WHNF t1 of
            T.Path _ (Just a) [b,c] -> do
                (tes, ty) <- typeCheckApps (getPos e1) ctx es $ Type (T.App a r2) lvl
                return (apps (T.At b c r1 r2) tes, ty)
            T.Path _ Nothing _ -> throwError [emsgLC (getPos e1) "Cannot infer type" enull]
            t1' -> throwError [emsgLC (getPos e1) "" $ pretty "Expected type: Path" $$
                                                       pretty "Actual type:" <+> prettyOpen ctx t1']
    go ctx e@E.Coe{} es Nothing | length es < 4 = throwError [emsgLC (getPos e) "Not implemented yet" enull] -- TODO
    go ctx e@E.Coe{} (e1:e2:e3:e4:es) Nothing = do
        (r1, _) <- go ctx e1 [] $ Just $ Type (T.Pi T.Interval $ ScopeTerm $ T.Universe NoLevel) (Level 1) -- TODO
        (r2, _) <- go ctx e2 [] $ Just $ Type T.Interval NoLevel
        (r3, _) <- go ctx e3 [] $ Just $ Type (nf WHNF $ T.App r1 r2) NoLevel -- TODO
        (r4, _) <- go ctx e4 [] $ Just $ Type T.Interval NoLevel
        (tes, ty) <- typeCheckApps (getPos e) ctx es $ Type (T.App r1 r4) NoLevel -- TODO
        return (T.Coe $ [r1,r2,r3,r4] ++ tes, ty)
    go ctx e@E.Iso{} es Nothing | length es < 7 = throwError [emsgLC (getPos e) "Not implemented yet" enull] -- TODO
    go ctx E.Iso{} [e1,e2,e3,e4,e5,e6,e7] Nothing = do
        (r1, Type t1 _) <- go ctx e1 [] Nothing
        (r2, Type t2 _) <- go ctx e2 [] Nothing
        let t1' = nf WHNF t1
            t2' = nf WHNF t2
        lvl <- checkUniverses ctx ctx e1 e2 t1' t2'
        (r3, _)  <- go ctx e3 [] $ Just $ Type (T.Pi r1 $ ScopeTerm r2) lvl
        (r4, _)  <- go ctx e4 [] $ Just $ Type (T.Pi r2 $ ScopeTerm r1) lvl
        let h e s1 s3 s4 = \(T.Universe tlvl) -> go ctx e [] $ Just $ Type (T.Pi s1 $ Scope "x" $
                ScopeTerm $ T.Path Implicit (Just $ T.Pi T.Interval $ ScopeTerm $ fmap Free s1)
                [T.App (fmap Free s4) $ T.App (fmap Free s3) $ T.Var Bound, T.Var Bound]) tlvl
        (r5, _) <- h e5 r1 r3 r4 t1'
        (r6, _) <- h e6 r2 r4 r3 t2'
        (r7, _) <- go ctx e7 [] $ Just $ Type T.Interval NoLevel
        return (T.Iso [r1,r2,r3,r4,r5,r6,r7], Type (T.Universe lvl) $ succ lvl)
    go ctx e@E.Squeeze{} es Nothing | length es < 2 = throwError [emsgLC (getPos e) "Not implemented yet" enull] -- TODO
    go ctx E.Squeeze{} [e1,e2] Nothing = do
        (r1, _) <- go ctx e1 [] $ Just (Type T.Interval NoLevel)
        (r2, _) <- go ctx e2 [] $ Just (Type T.Interval NoLevel)
        return (T.Squeeze [r1,r2], Type T.Interval NoLevel)
    go ctx (E.Pi [] e) [] Nothing = go ctx e [] Nothing
    go ctx expr@(E.Pi (PiTele _ e1 e2 : tvs) e) [] Nothing = do
        args <- exprToVars e1
        (r1, Type t1 lvl1) <- go ctx e2 [] Nothing
        case abstractCtx (map unArg args) Nil (Type r1 lvl1) of
            SomeEq ctx' -> do
                (r2, Type t2 _) <- go (ctx +++ ctx') (E.Pi tvs e) [] Nothing
                lvl <- checkUniverses ctx (ctx +++ ctx') e2 (E.Pi tvs e) (nf WHNF t1) (nf WHNF t2)
                return (T.Pi r1 $ abstractTermInCtx ctx' r2, Type (T.Universe lvl) $ succ lvl)
      where
        abstractCtx :: Eq a => [s] -> Ctx s Type b a -> Type a -> SomeEq (Ctx s Type b)
        abstractCtx [] ctx _ = SomeEq ctx
        abstractCtx (x:xs) ctx t = abstractCtx xs (Snoc ctx x t) (fmap Free t)
    go ctx (E.Arr e1 e2) [] Nothing = do
        (r1, Type t1 _) <- go ctx e1 [] Nothing
        (r2, Type t2 _) <- go ctx e2 [] Nothing
        lvl <- checkUniverses ctx ctx e1 e2 (nf WHNF t1) (nf WHNF t2)
        return (T.Pi r1 $ ScopeTerm r2, Type (T.Universe lvl) $ succ lvl)
    go _ (E.Universe (U (_,u))) [] Nothing =
        let l = parseLevel u
            l' = Level (level l + 1)
        in return (T.Universe l, Type (T.Universe l') $ succ l')
      where
        parseLevel :: String -> Level
        parseLevel "Type" = NoLevel
        parseLevel ('T':'y':'p':'e':s) = Level (read s)
        parseLevel s = error $ "parseLevel: " ++ s
    go _ E.Interval{} [] Nothing = return (T.Interval, Type (T.Universe NoLevel) $ Level 1)
    go ctx e@E.Path{} [] Nothing = throwError [inferErrorMsg (getPos e) "Path"]
    go ctx e@E.Path{} [] (Just ty) = throwError [emsgLC (getPos e) "Not implemented yet" enull] -- TODO
    go ctx E.Path{} (e1:es) Nothing | length es < 3 = do
        let lvl = NoLevel -- TODO
        (r1, _) <- go ctx e1 [] $ Just $ Type (T.Pi T.Interval $ ScopeTerm $ T.Universe lvl) (succ lvl)
        case es of
            [] -> return (T.Path Explicit (Just r1) [], Type (T.Pi (T.App r1 $ ICon ILeft) $
                ScopeTerm $ T.Pi (T.App r1 $ ICon IRight) $ ScopeTerm $ T.Universe lvl) $ succ lvl)
            [e2] -> do
                (r2,_) <- go ctx e2 [] $ Just $ Type (nf WHNF $ T.App r1 $ ICon ILeft) lvl
                return (T.Path Explicit (Just r1) [r2], Type (T.Pi (T.App r1 $ ICon IRight) $ ScopeTerm $ T.Universe lvl) $ succ lvl)
            [e2,e3] -> do
                (r2,_) <- go ctx e2 [] $ Just $ Type (nf WHNF $ T.App r1 $ ICon ILeft) lvl
                (r3,_) <- go ctx e3 [] $ Just $ Type (nf WHNF $ T.App r1 $ ICon IRight) lvl
                return (T.Path Explicit (Just r1) [r2,r3], Type (T.Universe lvl) $ succ lvl)
            _ -> error "typeCheckCtx.Path"
    go ctx (E.PathImp e1 e2) [] Nothing = do
        (r1, Type t1 lvl) <- go ctx e1 [] Nothing
        (r2, _) <- go ctx e2 [] $ Just $ Type (nf WHNF t1) lvl
        return (T.Path Implicit (Just $ T.Lam $ Scope1 "_" $ fmap Free t1) [r1,r2], Type (T.Universe lvl) $ succ lvl)
    go _ e _ Nothing = throwError [emsgLC (getPos e) "A type is applied to arguments" enull]
    go ctx e es (Just (Type ty lvl)) = do
        (r, Type t _) <- go ctx e es Nothing
        actExpType ctx t ty (getPos e)
        return (r, Type ty lvl)

actExpType :: (Monad m, Eq a) => Ctx String Type String a -> Term a -> Term a -> (Int,Int) -> EDocM m ()
actExpType ctx act exp lc =
    let act' = nf NF act
        exp' = nf NF exp
    in unless (act' `lessOrEqual` exp') $
        throwError [emsgLC lc "" $ pretty "Expected type:" <+> prettyOpen ctx exp' $$
                                   pretty "Actual type:"   <+> prettyOpen ctx act']

typeCheckApps :: (Monad m, Eq a) => (Int,Int) -> Ctx String Type String a -> [Expr] -> Type a -> TCM m ([Term a], Type a)
typeCheckApps lc ctx exprs (Type ty lvl) = go exprs (nf WHNF ty)
  where
    go [] ty = return ([], Type ty lvl)
    go (expr:exprs) (T.Pi a b) = do
        (term, _)   <- typeCheckCtx ctx expr $ Just (Type a lvl)
        (terms, ty) <- go exprs $ instantiate1 term (dropOnePi a b)
        return (term:terms, ty)
    go _ ty = throwError [emsgLC lc "" $ pretty "Expected pi type" $$
                                         pretty "Actual type:" <+> prettyOpen ctx ty]
