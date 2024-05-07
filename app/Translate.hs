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
genExpr (EInl e) = do
    (stmts, loc) <- genExpr e
    pure (stmts, LocSum (LocBool False) loc (LocUndefined)) -- FIXME tag
genExpr (EInr e) = do
    (stmts, loc) <- genExpr e
    pure (stmts, LocSum (LocBool True) (LocUndefined) loc) -- FIXME tag
genExpr (ECall name args) = do
    (argCodes, argLocs) <- unzip <$> mapM genExpr args
    let argsCode = concat argCodes
    let yulArgs = flattenArgs argLocs
    info <- lookupFun name
    let resultType = fun_result info
    (resultCode, resultLoc) <- allocResult resultType
    let callExpr = YulCall name yulArgs
    let callCode = [YulAssign (flattenRes resultLoc) callExpr]
    pure (argsCode++resultCode++callCode, resultLoc)
    where
        flattenArgs :: [Location] ->  [YulExpression]
        flattenArgs = concatMap flattenArg
        flattenArg :: Location -> [YulExpression]
        flattenArg (LocInt n) = [YulLiteral (YulNumber (fromIntegral n))]
        flattenArg (LocBool b) = [YulLiteral (if b then YulTrue else YulFalse)]
        flattenArg (LocStack i) = [YulIdentifier (stkLoc i)]
        flattenArg (LocPair l r) = flattenArg l ++ flattenArg r
        flattenArg l = error("flattenArg: not implemented for "++show l)
        flattenRes :: Location -> [Name]
        flattenRes (LocStack i) = [stkLoc i]
        flattenRes (LocPair l r) = flattenRes l ++ flattenRes r
        flattenRes l = error("flattenRes: not implemented for "++show l)
        allocResult :: Type -> TM ([YulStatement], Location)
        allocResult TInt = do
            n <- freshId
            let loc = LocStack n
            pure ([YulAlloc (stkLoc n)], loc)
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
genStmt stmt@(SReturn expr) = do
    (stmts, loc) <- genExpr expr
    resultLoc <- lookupVar "_result"
    writeln $ show stmt ++  " - result at " ++ show resultLoc
    let stmts' = copyLocs resultLoc loc
    pure (stmts ++ stmts')
    {-
    case loc of
        LocStack i -> pure (stmts ++ [YulAssign ["_result"] (YulIdentifier (stkLoc i))])
        LocInt n -> pure (stmts ++ [YulAssign ["_result"] (YulLiteral (YulNumber (fromIntegral n)))])
        _ -> error "SReturn: type mismatch"
    -}
genStmt (SBlock stmts) = genStmts stmts
genStmt (SCase e alts) = do
    (stmts, loc) <- genExpr e
    case loc of
        LocSum loctag l r -> do
            yulAlts <- genAlts l r alts
            pure (stmts ++ [YulSwitch (yultag loctag) yulAlts Nothing]) where
                yultag (LocStack i) = YulIdentifier (stkLoc i)
                yultag (LocBool b) = YulLiteral (if b then YulTrue else YulFalse)
                yultag (LocInt n) = YulLiteral (YulNumber (fromIntegral n))
        _ -> error "SCase: type mismatch"

genStmt (SFunction name args ret stmts) = do
    let argTypes = map (\(TArg _ t) -> t) args
    let info = FunInfo argTypes ret
    insertFun name info
    withLocalEnv do
        yulArgs <- placeArgs args
        yulResult <- placeResult ret
        yulBody <- genStmts stmts
        return [YulFun name yulArgs (YReturns yulResult) yulBody]
    where
        placeArgs :: [Arg] -> TM [YArg]
        placeArgs = mapM placeArg
        placeArg :: Arg -> TM YArg
        placeArg (TArg name TInt) = do
            n <- freshId
            let loc = LocStack n
            insertVar name loc
            return (stkLoc n)
        placeArg (TArg _ typ) = error("placeArg: not implemented for type" ++ show typ)
        placeResult :: Type -> TM [YArg]
        placeResult TInt = do
            n <- freshId
            let loc = LocStack n
            insertVar "_result" loc
            return [stkLoc n]

genStmt e = error $ "genStmt unimplemented for: " ++ show e

genAlts :: Location -> Location -> [Alt] -> TM [(YulLiteral, [YulStatement])]
genAlts locL locR [(Alt lname lstmt), (Alt rname rstmt)] = do
    yulLStmts <- withName lname locL lstmt
    yulRStmts <- withName rname locR rstmt
    pure [(YulFalse, yulLStmts), (YulTrue, yulRStmts)]
    where
        withName name loc stmt = withLocalEnv do
            insertVar name loc
            bstmts <- genStmt stmt
            pure bstmts
genAlts _ _ _ = error "genAlts: invalid number of alternatives"

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
    let stmts3 = copyLocs locLhs locRhs
    pure (stmts1 ++ stmts2 ++ stmts3)

loadLoc :: Location -> YulExpression
loadLoc (LocInt n) = YulLiteral (YulNumber (fromIntegral n))
loadLoc (LocBool b) = YulLiteral (if b then YulTrue else YulFalse)
loadLoc (LocStack i) = YulIdentifier (stkLoc i)
loadLoc loc = error("cannot loadLoc "++show loc)

-- copyLocs l r copies the value of r to l
copyLocs :: Location -> Location -> [YulStatement]
copyLocs (LocStack i) r@(LocInt _) = [YulAssign [stkLoc i] (loadLoc r)]
copyLocs (LocStack i) r@(LocBool _) = [YulAssign [stkLoc i] (loadLoc r)]
copyLocs (LocStack i) r@(LocStack _) = [YulAssign [stkLoc i] (loadLoc r)]
copyLocs (LocStack _) LocUndefined = [YulComment "impossible"]
copyLocs (LocPair l1 l2) (LocPair r1 r2) = copyLocs l1 r1 ++ copyLocs l2 r2
copyLocs (LocSum ltag l1 l2) (LocSum rtag r1 r2) =  copyLocs ltag rtag ++ (copySum rtag) where
    copySum (LocBool b) = case b of
        False -> copyLocs l1 r1   -- explicit inl
        True -> copyLocs l2 r2    -- explicit inr

    copySum (LocStack i) = [YulSwitch (YulIdentifier (stkLoc i))
                [ (YulNumber 0, copyLocs l1 r1)
                , (YulNumber 1, copyLocs l2 r2)
                ]
                Nothing]
    copySum l = error("Invalid tag location: "++show l)
copyLocs l r = error $ "copy: type mismatch - LHS: " ++ show l ++ " RHS: " ++ show r

genStmts :: [Stmt] -> TM [YulStatement]
genStmts stmts = concat <$> mapM genStmtWithComment stmts

translateCore :: Core -> TM Yul
translateCore (Core stmts) = do
    -- assuming the result goes into `_mainresult`
    n <- freshId
    let prolog = [YulAlloc (stkLoc n)]
    insertVar "_result" (LocStack n)
    mainBody <- genStmts stmts
    let epilog = [YulAssign ["_mainresult"] (YulIdentifier (stkLoc n))]
    return $ Yul(prolog ++ mainBody ++ epilog )
