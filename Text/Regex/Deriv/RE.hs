
{-# LANGUAGE GADTs, MultiParamTypeClasses, FunctionalDependencies,
    FlexibleInstances, TypeSynonymInstances, FlexibleContexts #-} 

module Text.Regex.Deriv.RE where

import Data.List (nub)
import Data.Char (chr)

import Text.Regex.Deriv.Common (PosEpsilon(..), IsEpsilon(..), IsPhi(..), Simplifiable(..), IsGreedy(..), GFlag(..))
import Text.Regex.Deriv.Dictionary (Key(..), primeL, primeR)

-- ------------------------

-- | data type of the regular expresions
data RE = Phi 
 | Empty        -- ^ an empty exp
 | L Char -- ^ a literal / a character
 | Choice [RE] GFlag -- ^ a choice exp 'r1 + r2'
 | ChoiceInt [RE] -- ^ internal choice used in the BitCode version
 | Seq RE RE     -- ^ a pair exp '(r1,r2)'
 | Star RE GFlag -- ^ a kleene's star exp 'r*'
 | Any           -- ^ .
 | Not [Char]    -- ^ excluding characters e.g. [^abc]
  deriving Show

-- | the eq instance
instance Eq RE where
    (==) Empty Empty = True
    (==) (L x) (L y) = x == y
    (==) (Choice rs1 g1) (Choice rs2 g2) = (g1 == g2) && (rs2 == rs1) 
    (==) (ChoiceInt rs1) (ChoiceInt rs2) = (rs2 == rs1) 
    (==) (Seq r1 r2) (Seq r3 r4) = (r1 == r3) && (r2 == r4)
    (==) (Star r1 g1) (Star r2 g2) = g1 == g2 && r1 == r2 
    (==) Any Any = True
    (==) (Not cs) (Not cs') = cs == cs' 
    (==) Phi Phi = True
    (==) _ _ = False


instance Ord RE where
    compare Empty Empty = {-# SCC "compare0" #-} EQ
    compare (L x) (L y) = {-# SCC "compare1" #-} compare x y
    compare (Choice rs1 _) (Choice rs2 _) =  
        let l1 = length rs1   
            l2 = length rs2
            -- rs1' = reverse rs1
            -- rs2' = reverse rs2
        in if l1 == l2
           then
              {-# SCC "compare2" #-} compare rs1 rs2
           else compare l1 l2 
    compare (ChoiceInt rs1) (ChoiceInt rs2) =  
        let l1 = length rs1   
            l2 = length rs2
            -- rs1' = reverse rs1
            -- rs2' = reverse rs2
        in if l1 == l2
           then
              {-# SCC "compare2" #-} compare rs1 rs2
           else compare l1 l2 
    compare (Seq r1 r2) (Seq r3 r4) = 
      let x = {-# SCC "compare3" #-} compare r1 r3
      in x `seq` case x of 
      { EQ -> {-# SCC "compare4" #-} compare r2 r4
      ; _  -> x }
    compare (Star r1 _) (Star r2 _) = {-# SCC "compare5" #-} compare r1 r2
    compare Any Any = {-# SCC "compare6" #-} EQ
    compare (Not cs) (Not cs') = compare cs cs'
    compare r1 r2 = {-# SCC "compare7" #-} compare (assignInt r1) (assignInt r2)                                                                              

assignInt :: RE -> Int
assignInt Empty = 0
assignInt (L _) = 1             
assignInt (Choice _ _) = 2
assignInt (Seq _ _) = 3
assignInt (Star _ _) = 4
assignInt Any = 5
assignInt (Not _) = 6
assignInt (ChoiceInt _) = 7
assignInt Phi = 8
{-
-- | A pretty printing function for regular expression
instance Show RE where
    show Phi = "{}"
    show Empty = "<>"
    show (L c) = show c
    show (Choice rs g) = "(" ++ show rs ++ ")" ++ show g
    show (ChoiceInt rs) = "(i:" ++ show rs ++ ":i)"
    show (Seq r1 r2) = "<" ++ show r1 ++ "," ++ show r2 ++ ">"
    show (Star r g) = show r ++ "*" ++ show g
    show Any = "."
    show (Not cs) = "[^" ++ cs ++ "]"
-}

instance IsGreedy RE where
    isGreedy Phi = True
    isGreedy Empty = False
    isGreedy (Choice _ Greedy) = True
    isGreedy (Choice _ NotGreedy) = False -- (isGreedy r1) || (isGreedy r2)
    isGreedy (Seq r1 r2) = (isGreedy r1) || (isGreedy r2)
    isGreedy (Star _ Greedy) = True
    isGreedy (Star _ NotGreedy) = False
    isGreedy (L _) = True
    isGreedy Any = True
    isGreedy (Not _) = True

instance Key RE where
    hash Phi = [0]
    hash Empty = [1]
    hash (Choice _ Greedy) = {- let x1 = head (hash r1)
                                     x2 = head (hash r2)
                                 in [ 3 +  x1 * primeL + x2 * primeR ] -} [3]
    hash (Choice _ NotGreedy) = {- let x1 = head (hash r1)
                                        x2 = head (hash r2)
                                    in [ 4 + x1 * primeL + x2 * primeR ] -} [4]
    hash (Seq r1 r2) = let x1 = head (hash r1)
                           x2 = head (hash r2)
                       in [ 5 + x1 * primeL + x2 * primeR ] --  [5]
    hash (Star _ Greedy) = {- let x = head (hash r)
                           in [ 6 + x * primeL ] -} [6]
    hash (Star _ NotGreedy) = {- let x = head (hash r)
                            in [ 7 + x * primeL ] -} [7]
    hash (L _) = {- let x = head (hash c)
                 in [ 8 + x * primeL ] -} [8]
    hash Any = [2]
    hash (Not _) = [9]



-- | function 'resToRE' sums up a list of regular expressions with the choice operation.
resToRE :: [RE] -> RE
resToRE x@(_:_) = Choice x Greedy
resToRE [] = Phi


instance PosEpsilon RE where
  posEpsilon Phi = False
  posEpsilon Empty = True
  posEpsilon (Choice rs _) = any posEpsilon rs
  posEpsilon (ChoiceInt rs) = any posEpsilon rs
  posEpsilon (Seq r1 r2) = (posEpsilon r1) && (posEpsilon r2)
  posEpsilon (Star _ _) = True
  posEpsilon (L _) = False
  posEpsilon Any = False
  posEpsilon (Not _) = False
--         

-- | function 'isEpsilon' checks whether epsilon = r
instance IsEpsilon RE where
  isEpsilon Phi = False
  isEpsilon Empty = True
  isEpsilon (Choice rs _) = all isEpsilon rs
  isEpsilon (Seq r1 r2) = (isEpsilon r1) && (isEpsilon r2)
  isEpsilon (Star Phi _) = True
  isEpsilon (Star r _) = isEpsilon r
  isEpsilon (L _) = False
  isEpsilon Any = False
  isEpsilon (Not _) = False

instance IsPhi RE where
  isPhi Phi = True
  isPhi Empty = False
  isPhi (Choice [] _) = True
  isPhi (Choice rs _) = all isPhi rs
  isPhi (ChoiceInt []) = True
  isPhi (ChoiceInt rs) = all isPhi rs
  isPhi (Seq r1 r2) = (isPhi r1) || (isPhi r2)
  isPhi (Star _ _) = False
  isPhi (L _) = False
  isPhi Any = False
  isPhi (Not _) = False

-- | function 'partDeriv' implements the partial derivative operations for regular expressions. We don't pay attention to the greediness flag here.
partDeriv :: RE -> Char -> [RE]
partDeriv r l = let pds = (partDerivSub r l)
                in {-# SCC "nub_pd" #-} nub (map simplify pds)                                                 


partDerivSub :: RE -> Char -> [RE]
partDerivSub Phi _ = []
partDerivSub Empty _ = []
partDerivSub (L l') l 
    | l == l'   = [Empty]
    | otherwise = []
partDerivSub Any _ = [Empty]
partDerivSub (Not cs) l 
    | l `elem` cs = []
    | otherwise = [Empty]
partDerivSub (Choice rs _) l = concatMap (\ r -> partDerivSub r l) rs
partDerivSub (Seq r1 r2) l 
    | posEpsilon r1 = 
          let 
              s0 = partDerivSub r1 l
              s1 = s0 `seq` [ (Seq r1' r2) | r1' <- s0 ]
              s2 = partDerivSub r2 l
          in s1 `seq` s2 `seq` (s1 ++ s2)
    | otherwise = 
        let 
            s0 = partDerivSub r1 l 
        in s0 `seq` [ (Seq r1' r2) | r1' <- s0 ]
partDerivSub (Star r g) l = 
    let
        s0 = partDerivSub r l
    in s0 `seq` [ (Seq r' (Star r g)) | r' <- s0 ]

-- | function 'sigmaRE' returns all characters appearing in a reg exp.
sigmaRE :: RE -> [Char]
sigmaRE r = let s = (sigmaREsub r)
            in s `seq` nub s

sigmaREsub :: RE -> [Char]
sigmaREsub (L l) = [l]
sigmaREsub Any = map chr [32 .. 127]
sigmaREsub (Not cs) = filter (\c -> not (c `elem` cs)) (map chr [32 .. 127])
sigmaREsub (Seq r1 r2) = (sigmaREsub r1) ++ (sigmaREsub r2) 
sigmaREsub (Choice rs _) = concatMap sigmaREsub rs
sigmaREsub (Star r _) = sigmaREsub r
sigmaREsub Phi = []
sigmaREsub Empty = []

instance Simplifiable RE where
    simplify (L l) = L l
    simplify Any   = Any
    simplify (Not cs) = Not cs
    simplify (Seq r1 r2) = 
        let r1' = simplify r1
            r2' = simplify r2
        in if isEpsilon r1'
           then r2'
           else if isEpsilon r2'
                then r1'
                else Seq r1' r2'
    simplify (Choice rs g) = 
        let rs' = filter (not . isPhi) $ map simplify rs
        in if null rs' 
           then Phi
           else Choice rs' g
    simplify (Star r g) = Star (simplify r) g
    simplify Phi = Phi
    simplify Empty = Empty
