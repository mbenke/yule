module Locus where

{-
Location tree with addresses a:
- location for Int is a single cell
- location for pair is a pair of locations for components
- location for sum is a location for tag and locations for components
-}
data LocTree a 
    = LocInt Int
    | LocStack Int
    | LocPair (LocTree a) (LocTree a)
    | LocSum a (LocTree a) (LocTree a)
    deriving (Show)

type Location = LocTree Int

stkLoc :: Int -> String
stkLoc i = "_v" ++ show i