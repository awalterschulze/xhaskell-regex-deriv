-- | this module contains the defs of common data types and type classes

module Text.Regex.Deriv.Common
    ( Range(..), range, minRange, maxRange
    , Letter
    , PosEpsilon (..)
    , IsEpsilon (..)
    , IsPhi (..)
    , Simplifiable (..)
    , myHash
    , myLookup
    , GFlag (..)
    , IsGreedy (..)
    , nub2
    , nub3
    , preBinder
    , preBinder_
    , subBinder
    , mainBinder
    ) where

import Data.Char (ord)
import qualified Data.IntMap as IM

-- | (sub)words represent by range
-- type Range  = (Int,Int)
data Range = Range !Int !Int deriving (Show, Ord)

instance Eq Range where
  (==) (Range x y) (Range w z) = (x == w) && (y == z)

range :: Int -> Int -> Range
range = Range

minRange :: (Int, Int) -> Int
minRange = fst
maxRange :: (Int, Int) -> Int
maxRange = snd

-- | a character and its index (position)
type Letter = (Char,Int)

-- | test for 'epsilon \in a' epsilon-possession
class PosEpsilon a where
    posEpsilon :: a -> Bool

-- | test for epsilon == a
class IsEpsilon a where
    isEpsilon :: a -> Bool

-- | test for \phi == a
class IsPhi a where
    isPhi :: a -> Bool

class Simplifiable a where
    simplify :: a -> a

myHash :: Int -> Char -> Int
myHash i x = ord x + 256 * i

-- the lookup function

myLookup :: Int -> Char -> IM.IntMap [Int] -> [Int]
myLookup i x dict = case IM.lookup (myHash i x) dict of
                     Just ys -> ys
                     Nothing -> []

-- | The greediness flag
data GFlag = Greedy    -- ^ greedy
           | NotGreedy -- ^ not greedy
             deriving (Eq,Ord)

instance Show GFlag where
    show Greedy = ""
    show NotGreedy = "?"

class IsGreedy a where
    isGreedy :: a -> Bool

-- remove duplications in a list of pairs, using the first components as key.
nub2 :: [(Int,a)] -> [(Int,a)]
nub2 [] = []
nub2 [x] = [x]
nub2 ls = nub2sub IM.empty ls

nub2sub :: IM.IntMap () -> [(IM.Key, t)] -> [(IM.Key, t)]
nub2sub _ [] = []
nub2sub im (x@(k,_):xs) =
           case IM.lookup k im of
           Just _  -> xs `seq` nub2sub im xs
           Nothing -> let im' = IM.insert k () im
                      in im' `seq` xs `seq` x:nub2sub im' xs

nub3 :: [(Int,a,Int)] -> [(Int,a,Int)]
nub3 [] = []
nub3 [x] = [x]
nub3 ls = nub3subsimple IM.empty ls

nub3subsimple :: IM.IntMap () -> [(Int,a,Int)] -> [(Int,a,Int)]
nub3subsimple _ [] = []
nub3subsimple _ [ x ] = [ x ]
nub3subsimple im (x@(_,_,0):xs) = x:(nub3subsimple im xs)
nub3subsimple im (x@(k,_,1):xs) = let im' = IM.insert k () im
                                  in im' `seq` x:(nub3subsimple im' xs)
nub3subsimple im (x@(k,_,_):xs) = case IM.lookup k im of
                                  Just _ -> nub3subsimple im xs
                                  Nothing -> let im' = IM.insert k () im
                                             in im' `seq` xs `seq` x:(nub3subsimple im' xs)



-- The smallest binder index capturing the prefix of the unanchored regex

preBinder :: Int
preBinder = -1

preBinder_ :: Int
preBinder_ = -2

-- The largest binder index capturing for the suffix of the unanchored regex

subBinder :: Int
subBinder = 2147483647


-- The binder index capturing substring which matches by the unanchored regex

mainBinder :: Int
mainBinder = 0
