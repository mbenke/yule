module Translate where

import Core
import TM
import Yul


genExpr :: Expr -> TM ([YulStatement], Location)
genExpr (EInt n) = pure ([], LocInt n)
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

genExpr e = error("genExpr: not implemented for"++show e)

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
genStmt (SReturn name) = do
    loc <- lookupVar name
    case loc of
        LocStack i -> pure [YulAssign ["_result"] (YulIdentifier (stkLoc i))]
        LocInt n -> pure [YulAssign ["_result"] (YulLiteral (YulNumber (fromIntegral n)))]
        _ -> error "SReturn: type mismatch"
genStmt e = error $ "genStmt: " ++ show e

coreAlloc :: Name -> Type -> TM [YulStatement]
coreAlloc name typ = do
    (stmts, loc) <- alloc typ
    insertVar name loc
    return stmts
    where
        alloc TInt = do
            n <- freshId
            let loc = LocStack n
            pure ([YulAlloc (stkLoc n)], loc)
        alloc (TPair t1 t2) = do
            (stmts1, loc1) <- alloc t1
            (stmts2, loc2) <- alloc t2
            pure (stmts1 ++ stmts2, LocPair loc1 loc2)
        alloc t = error("cannot allocate "++show t)


coreAssign :: Expr -> Expr -> TM [YulStatement]
coreAssign lhs rhs = do
    (stmts1, locLhs) <- genExpr lhs
    (stmts2, locRhs) <- genExpr rhs
    let stmts3 = copy locLhs locRhs
    pure (stmts1 ++ stmts2 ++ stmts3)
  where
    load (LocInt n) = YulLiteral (YulNumber (fromIntegral n))
    load (LocStack i) = YulIdentifier (stkLoc i)
    load loc = error("cannot load "++show loc)
    copy (LocStack i) r@(LocInt _) = [YulAssign [stkLoc i] (load r)]
    copy (LocStack i) r@(LocStack _) = [YulAssign [stkLoc i] (load r)]
    copy (LocPair l1 l2) (LocPair r1 r2) = copy l1 r1 ++ copy l2 r2
    copy _ _ = error "copy: type mismatch"

genStmts :: [Stmt] -> TM Yul
genStmts stmts = Yul . concat <$> mapM genStmtWithComment stmts

translateCore :: Core -> TM Yul
translateCore (Core stmts) = genStmts stmts
