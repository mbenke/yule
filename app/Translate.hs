module Translate where

import Core
import TM
import Yul


genExpr :: Expr -> TM ([YulStatement], Location)
genExpr (EInt n) = pure ([], LocInt n)
genExpr (EBool b) = pure ([], LocBool b)
genExpr (EVar name) = do
    loc <- lookupVar name
    pure ([], loc)
genExpr (EPair e1 e2) = do
    (stmts1, loc1) <- genExpr e1
    (stmts2, loc2) <- genExpr e2
    pure (stmts1 ++ stmts2, LocPair loc1 loc2)
genExpr (EFst e) = do
    (stmts, loc) <- genExpr e
    case loc of
        LocPair l _ -> pure (stmts, l)
        _ -> error "EFst: type mismatch"
genExpr (ESnd e) = do
    (stmts, loc) <- genExpr e
    case loc of
        LocPair _ r -> pure (stmts, r)
        _ -> error "ESnd: type mismatch"
genExpr (EInl tl tr e) = do
    (stmts, loc) <- genExpr e
    pure (stmts, LocSum (LocBool False) loc (LocUndefined)) -- FIXME tag
genExpr (EInr tl tr e) = do
    (stmts, loc) <- genExpr e
    pure (stmts, LocSum (LocBool True) (LocUndefined) loc) -- FIXME tag

genExpr e = error("genExpr: not implemented for "++show e)

genStmtWithComment :: Stmt -> TM [YulStatement]
genStmtWithComment (SComment c) = pure [YulComment c]
genStmtWithComment s = do
    let comment = YulComment(show s)
    body <- genStmt s
    pure (comment : body)

genStmt :: Stmt -> TM [YulStatement]
genStmt (SAssembly stmts) = pure stmts
genStmt (SAlloc name typ) = coreAlloc name typ
genStmt (SAssign name expr) = coreAssign name expr
{-
genStmt (SReturn name) = do
    loc <- lookupVar name
    case loc of
        LocStack i -> pure [YulAssign ["_result"] (YulIdentifier (stkLoc i))]
        LocInt n -> pure [YulAssign ["_result"] (YulLiteral (YulNumber (fromIntegral n)))]
        _ -> error "SReturn: type mismatch"
-}
genStmt (SReturn expr) = do
    (stmts, loc) <- genExpr expr
    case loc of
        LocStack i -> pure (stmts ++ [YulAssign ["_result"] (YulIdentifier (stkLoc i))])
        LocInt n -> pure (stmts ++ [YulAssign ["_result"] (YulLiteral (YulNumber (fromIntegral n)))])
        _ -> error "SReturn: type mismatch"
genStmt (SBlock stmts) = yulStmts <$> genStmts stmts
genStmt (SCase e alts) = do
    (stmts, loc) <- genExpr e
    case loc of
        LocSum loctag l r -> do
            yulAlts <- genAlts alts
            pure (stmts ++ [YulSwitch (yultag loctag) yulAlts Nothing]) where
                yultag (LocStack i) = YulIdentifier (stkLoc i)
                yultag (LocBool b) = YulLiteral (if b then YulTrue else YulFalse)
                yultag (LocInt n) = YulLiteral (YulNumber (fromIntegral n))
        _ -> error "SCase: type mismatch"
genStmt e = error $ "genStmt unimplemented for: " ++ show e

genAlts :: [Alt] -> TM [(YulLiteral, [YulStatement])]
genAlts alts = pure [] -- FIXME: implement

coreAlloc :: Name -> Type -> TM [YulStatement]
coreAlloc name typ = do
    (stmts, loc) <- alloc typ
    insertVar name loc
    return stmts
    where
        alloc :: Type -> TM ([YulStatement], Location)
        alloc TInt = do
            n <- freshId
            let loc = LocStack n
            pure ([YulAlloc (stkLoc n)], loc)
        alloc TBool = allocBool
        alloc (TPair t1 t2) = do
            (stmts1, loc1) <- alloc t1
            (stmts2, loc2) <- alloc t2
            pure (stmts1 ++ stmts2, LocPair loc1 loc2)
        alloc (TSum t1 t2) = do
            (tagStmts, tagLoc) <- allocBool
            (stmts1, loc1) <- alloc t1
            (stmts2, loc2) <- alloc t2
            pure (tagStmts ++ stmts1 ++ stmts2, LocSum tagLoc loc1 loc2)
        alloc t = error("cannot allocate "++show t)
        allocBool :: TM ([YulStatement], Location)
        allocBool = do
            n <- freshId
            let loc = LocStack n
            pure ([YulAlloc (stkLoc n)], loc)


coreAssign :: Expr -> Expr -> TM [YulStatement]
coreAssign lhs rhs = do
    (stmts1, locLhs) <- genExpr lhs
    (stmts2, locRhs) <- genExpr rhs
    let stmts3 = copy locLhs locRhs
    pure (stmts1 ++ stmts2 ++ stmts3)
  where
    load :: Location -> YulExpression
    load (LocInt n) = YulLiteral (YulNumber (fromIntegral n))
    load (LocBool b) = YulLiteral (if b then YulTrue else YulFalse)
    load (LocStack i) = YulIdentifier (stkLoc i)
    load loc = error("cannot load "++show loc)
    copy :: Location -> Location -> [YulStatement]
    copy (LocStack i) r@(LocInt _) = [YulAssign [stkLoc i] (load r)]
    copy (LocStack i) r@(LocBool _) = [YulAssign [stkLoc i] (load r)]
    copy (LocStack i) r@(LocStack _) = [YulAssign [stkLoc i] (load r)]
    copy (LocStack _) LocUndefined = [YulComment "impossible"]
    copy (LocPair l1 l2) (LocPair r1 r2) = copy l1 r1 ++ copy l2 r2
    copy (LocSum ltag l1 l2) (LocSum rtag r1 r2) =  copy ltag rtag ++ (copySum rtag) where
        copySum (LocBool b) = case b of
            False -> copy l1 r1   -- explicit inl
            True -> copy l2 r2    -- explicit inr

        copySum (LocStack i) = [YulSwitch (YulIdentifier (stkLoc i))
                    [ (YulNumber 0, copy l1 r1)
                    , (YulNumber 1, copy l2 r2)
                    ]
                    Nothing]
        copySum l = error("Invalid tag location: "++show l)
    copy l r = error $ "copy: type mismatch - LHS: " ++ show l ++ " RHS: " ++ show r

genStmts :: [Stmt] -> TM Yul
genStmts stmts = Yul . concat <$> mapM genStmtWithComment stmts

translateCore :: Core -> TM Yul
translateCore (Core stmts) = genStmts stmts
