module TM
( TM
, runTM
, CEnv(..)
--, module RIO
, module Locus
, freshId
, lookupVar
, insertVar
, getEnv
, putEnv
, withLocalEnv
) where
import RIO
import qualified Data.Map as Map
import Data.Map(Map)

import Locus

type VarEnv = Map String Location
data CEnv = CEnv { env_counter :: IORef Int, env_vars :: IORef VarEnv }

type TM a = RIO CEnv a

runTM :: TM a -> IO a
runTM m = do
    counter <- newIORef 0
    vars <- newIORef Map.empty
    runRIO m (CEnv counter vars)

freshId :: TM Int
freshId = do
    counter <- reader env_counter
    n <- load counter
    store counter (n+1)
    return n

lookupVar :: String -> TM Location
lookupVar x = do
    vars <- getEnv
    case Map.lookup x vars of
        Just n -> return n
        Nothing -> error ("Variable not found: " ++ x)

insertVar :: String -> Location -> TM ()
insertVar x n = do
    vars <- reader env_vars
    m <- load vars
    store vars (Map.insert x n m)

{-
addFreshVar :: String -> TM String
addFreshVar x = do
    n <- freshId
    insertVar x n
    let freshVar = ('v':show n)
    info ["Adding fresh variable: ", x, " -> ", freshVar]
    return freshVar
-}
getEnv :: TM VarEnv
getEnv = do
    vars <- reader env_vars
    load vars

putEnv :: VarEnv -> TM ()
putEnv m = do
    vars <- reader env_vars
    store vars m

withLocalEnv :: TM a -> TM a
withLocalEnv m = do
    vars <- getEnv
    x <- m
    putEnv vars
    return x