module Translate where
import Data.List(union)
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
    let yulArgs = concatMap flattenRhs argLocs
    funInfo <- lookupFun name
    (resultCode, resultLoc) <- coreAlloc (fun_result funInfo)
    let callExpr = YulCall name yulArgs
    let callCode = [YulAssign (flattenLhs resultLoc) callExpr]
    pure (argsCode++resultCode++callCode, resultLoc)
genExpr e = error ("genExpr: not implemented for "++show e)

flattenRhs :: Location -> [YulExpression]
flattenRhs (LocInt n) = [yulInt n]
flattenRhs (LocBool b) = [yulBool b]
flattenRhs (LocStack i) = [YulIdentifier (stkLoc i)]
flattenRhs (LocPair l r) = flattenRhs l ++ flattenRhs r
flattenRhs l = error ("flattenRhs: not implemented for "++show l)

flattenLhs :: Location -> [Name]
flattenLhs (LocStack i) = [stkLoc i]
flattenLhs (LocPair l r) = flattenLhs l ++ flattenLhs r
flattenLhs l = error ("flattenLhs: not implemented for "++show l)

genStmtWithComment :: Stmt -> TM [YulStatement]
genStmtWithComment (SComment c) = pure [YulComment c]
genStmtWithComment s = do
    let comment = YulComment (show s)
    body <- genStmt s
    pure (comment : body)

genStmt :: Stmt -> TM [YulStatement]
genStmt (SAssembly stmts) = pure stmts
genStmt (SAlloc name typ) = allocVar name typ
genStmt (SAssign name expr) = coreAssign name expr

genStmt (SReturn expr) = do
    (stmts, loc) <- genExpr expr
    resultLoc <- lookupVar "_result"
    let stmts' = copyLocs resultLoc loc
    pure (stmts ++ stmts')

genStmt (SBlock stmts) = genStmts stmts
genStmt (SCase e alts) = do
    (stmts, loc) <- genExpr e
    case loc of
        LocSum loctag l r -> do
            yulAlts <- genAlts l r alts
            pure (stmts ++ [YulSwitch (yultag loctag) yulAlts Nothing]) where
                yultag (LocStack i) = YulIdentifier (stkLoc i)
                yultag (LocBool b) = yulBool b
                yultag (LocInt n) = yulInt n
                yultag t = error ("invalid tag: "++show t)
        _ -> error "SCase: type mismatch"

genStmt (SFunction name args ret stmts) = do
    let argTypes = map (\(TArg _ t) -> t) args
    let info = FunInfo argTypes ret
    insertFun name info
    withLocalEnv do
        yulArgs <- placeArgs args
        yulResult <- place "_result" ret  -- TODO: special handling of unit
        yulBody <- genStmts stmts
        return [YulFun name yulArgs (YReturns yulResult) yulBody]
    where
        placeArgs :: [Arg] -> TM [Name]
        placeArgs as = concat <$> mapM placeArg as
        placeArg :: Arg -> TM [Name]
        placeArg (TArg name typ) = place name typ
        place :: Name -> Type -> TM [Name]
        place name typ = do
            loc <- buildLoc typ
            insertVar name loc
            return (flattenLhs loc)


genStmt e = error $ "genStmt unimplemented for: " ++ show e

genAlts :: Location -> Location -> [Alt] -> TM [(YulLiteral, [YulStatement])]
genAlts locL locR [Alt lname lstmt, Alt rname rstmt] = do
    yulLStmts <- withName lname locL lstmt
    yulRStmts <- withName rname locR rstmt
    pure [(YulFalse, yulLStmts), (YulTrue, yulRStmts)]
    where
        withName name loc stmt = withLocalEnv do
            insertVar name loc
            genStmt stmt
genAlts _ _ _ = error "genAlts: invalid number of alternatives"


allocVar :: Name -> Type -> TM [YulStatement]
allocVar name typ = do
    (stmts, loc) <- coreAlloc typ
    insertVar name loc
    return stmts

buildLoc :: Type -> TM Location
buildLoc TInt = LocStack <$> freshId
buildLoc TBool = LocStack <$> freshId
buildLoc (TPair t1 t2) = do
    l1 <- buildLoc t1
    l2 <- buildLoc t2
    return (LocPair l1 l2)
buildLoc (TSum t1 t2) = do
    -- Make sum branches share stack slots
    tag <- LocStack <$> freshId
    mark <- getCounter
    l1 <- buildLoc t1
    leftMark <- getCounter
    setCounter mark
    l2 <- buildLoc t2
    rightMark <- getCounter
    setCounter (max leftMark rightMark)
    return (LocSum tag l1 l2)
buildLoc TUnit = pure LocUnit
buildLoc t = error ("cannot build location for "++show t)

coreAlloc :: Type -> TM ([YulStatement], Location)
coreAlloc t = do
    loc <- buildLoc t
    let stmts = allocLoc loc
    pure (stmts, loc)

stackSlots :: Location -> [Int]
stackSlots (LocStack i) = [i]
stackSlots (LocPair l r) = stackSlots l `union` stackSlots r
stackSlots (LocSum tag l r) = stackSlots tag `union` stackSlots l `union` stackSlots r
stackSlots _ = []

allocLoc :: Location -> [YulStatement]
allocLoc loc = [YulAlloc (stkLoc i) | i <- stackSlots loc]

allocWord :: TM ([YulStatement], Location)
allocWord = do
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
loadLoc loc = error ("cannot loadLoc "++show loc)

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
    copySum l = error ("Invalid tag location: "++show l)
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
    return $ Yul (prolog ++ mainBody ++ epilog )
