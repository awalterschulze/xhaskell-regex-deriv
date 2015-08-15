{-# LANGUAGE BangPatterns #-}
-- | This module defines the data type of internal regular expression pattern, 
-- | as well as the partial derivative operations for regular expression patterns.
module Text.Regex.Deriv.IntPattern 
    ( Pat(..)
    , strip
    , Binder
    , toBinder
    , listifyBinder
    , Key(..)
    )
    where

import Data.List
import qualified Data.IntMap as IM
import Text.Regex.Deriv.Common (Range(..), IsEpsilon(..), IsPhi(..), GFlag(..), IsGreedy(..), Simplifiable(..) )
import Text.Regex.Deriv.RE
import Text.Regex.Deriv.Dictionary (Key(..), primeL, primeR)
import Text.Regex.Deriv.Pretty


-- | regular expression patterns
data Pat = PVar Int [Range] Pat       -- ^ variable pattern 
  | PE [RE]                           -- ^ pattern without binder
  | PPair Pat Pat                     -- ^ pair pattern
  | PChoice [Pat] GFlag             -- ^ choice pattern 
  | PStar Pat GFlag                   -- ^ star pattern 
  | PPlus Pat Pat                     -- ^ plus pattern, it is used internally to indicate that it is unrolled from a PStar
  | PEmpty Pat                        -- ^ empty pattern, it is used intermally to indicate that mkEmpty function has been applied.
  deriving Show      

{-| The Eq instance for Pat data type
    NOTE: We ignore the 'consumed word' when comparing patterns
    (ie we only compare the pattern structure).
    Essential for later comparisons among patterns. -}

instance Eq Pat where
  (==) (PVar x1 _ p1) (PVar x2 _ p2) = (x1 == x2) && (p1 == p2) 
  (==) (PPair p1 p2) (PPair p1' p2') = (p1 == p1') && (p2 == p2')
  (==) (PChoice ps1 g1) (PChoice ps2 g2) = (g1 == g2) && (ps1 == ps2) -- more efficient, because choices are constructed in left-nested
  (==) (PPlus p1 p2) (PPlus p1' p2') = (p1 == p1') && (p2 == p2')
  (==) (PStar p1 g1) (PStar p2 g2) =  (g1 == g2) && (p1 == p2)
  (==) (PE rs1) (PE rs2) = rs1 == rs2
  (==) _ _ = False


instance Pretty a => Pretty [a] where
    pretty [] = "{}"
    -- pretty a@(x:xs) = "{" ++ prettyAll ++ "}" -- shadowing issue
    pretty (x:xs) = "{" ++ prettyAll ++ "}"
       where prettyAll = foldl' (\a i -> a++","++(pretty i)) (pretty x) xs

instance Pretty Pat where
    pretty (PVar x1 _ p1) = "(" ++ show x1 ++ ":" ++ pretty p1 ++ ")"
    pretty (PPair p1 p2) = "<" ++ pretty p1 ++ "," ++ pretty p2 ++ ">"
    pretty (PChoice ps g) = "(" ++ pretty ps ++ ")" ++ (show g)
    pretty (PE rs) = "|" ++ show rs ++ "|"
    pretty (PPlus p1 p2 ) = "(" ++ pretty p1 ++ "," ++ pretty p2 ++ ")"
    pretty (PStar p g) = (pretty p) ++ "*" ++ (show g)
    pretty (PEmpty p) = "[" ++ pretty p ++ "]"

{-
instance Show Pat where
    show pat = pretty pat
-}


instance Key Pat where
    hash (PVar x1 _ p1) = let y1 = head (hash x1) 
                              y2 = head (hash p1)
                          in y1 `seq` y2 `seq` [ 1 + y1 * primeL + y2 * primeR ] 
    hash (PPair p1 p2) = let x1 = head (hash p1)
                             x2 = head (hash p2)
                         in x1 `seq` x2 `seq` [ 2 + x1 * primeL + x2 * primeR ] 
    hash (PChoice (p1:p2:_) Greedy) = let x1 = head (hash p1)
                                          x2 = head (hash p2)
                                      in x1 `seq` x2 `seq`  [ 4 + x1 * primeL + x2 * primeR ] 
    hash (PChoice (p1:p2:_) NotGreedy) = let x1 = head (hash p1)
                                             x2 = head (hash p2)
                                         in x1 `seq` x2 `seq` [ 5 + x1 * primeL + x2 * primeR ]
    hash (PChoice (p1:_) _) = let x1 = head (hash p1)
                          
                              in x1 `seq`  [ 5 + x1 * primeL ]
    hash (PChoice [] _) = [5]
    hash (PPlus p1 p2) = let x1 = head (hash p1)
                             x2 = head (hash p2)
                         in x1 `seq` x2 `seq` [ 6 + x1 * primeL + x2 * primeR ]
    hash (PStar p Greedy) = let x = head (hash p)
                            in x `seq` [ 7 + x * primeL ]
    hash (PStar p NotGreedy) = let x = head (hash p)
                            in x `seq` [ 8 + x * primeL ]
    hash (PE r) = let x = head (hash r)
                  in x `seq` [ 9 + x * primeL ]
    hash (PEmpty p) = let x = head (hash p)
                      in x `seq` [ 3 + x * primeL ]
    hash p = error ("hash is applied to an unacceptable pattern " ++ (show p))

-- | function 'strip' strips away the bindings from a pattern
strip :: Pat -> RE 
strip (PVar _ _ p) = strip p
strip (PE rs) = resToRE rs
strip (PStar p g) = Star (strip p) g
strip (PPair p1 p2) = Seq (strip p1) (strip p2)
strip (PPlus p1 p2) = Seq (strip p1) (strip p2)
strip (PChoice ps g) = Choice (map strip ps) g
strip (PEmpty p) = strip p

-- | Function 'isGreedy' checks whether a pattern is greedy
instance IsGreedy Pat where
    isGreedy (PVar _ _ p) = isGreedy p
    isGreedy (PE rs) = any isGreedy rs
    isGreedy (PPair p1 p2) = isGreedy p1 || isGreedy p2
    isGreedy (PChoice _ Greedy) = True
    isGreedy (PChoice _ NotGreedy) = False -- isGreedy p1 || isGreedy p2
    isGreedy (PEmpty _) = False
    isGreedy (PStar _ Greedy) = True
    isGreedy (PStar _ NotGreedy) = False
    isGreedy (PPlus p p') = isGreedy p || isGreedy p'


-- | The 'Binder' type denotes a set of (pattern var * range) pairs
-- type Binder = [(Int, [Range])]
type Binder = IM.IntMap [Range]

-- | Function 'toBinder' turns a pattern into a binder
toBinder :: Pat -> Binder
toBinder p = IM.fromList (toBinderList p)

toBinderList :: Pat -> [(Int, [Range])]
toBinderList  (PVar i rs p) = [(i, rs)] ++ (toBinderList p)
toBinderList  (PPair p1 p2) = (toBinderList p1) ++ (toBinderList p2)
toBinderList  (PPlus p1 _) = (toBinderList p1) 
toBinderList  (PStar p1 _)    = (toBinderList p1) 
toBinderList  (PE _)        = []
toBinderList  (PChoice ps _) = concatMap toBinderList ps 
toBinderList  (PEmpty p) = toBinderList p

listifyBinder :: Binder -> [(Int, [Range])]
listifyBinder b = sortBy (\ x y -> compare (fst x) (fst y)) (IM.toList b)

-- | mainly interested in simplifying epsilon, p --> p
-- could be made more optimal, e.g. (epsilon, epsilon) --> epsilon
instance Simplifiable Pat where
    -- simplify :: Pat -> Pat
    simplify (PVar i rs p) = PVar i rs (simplify p)
    simplify (PPair p1 p2) =
        let p1' = simplify p1
            p2' = simplify p2
        in if isEpsilon p1'
           then p2'
           else if isEpsilon p2'
                then p1'
                else PPair p1' p2'
    simplify (PChoice ps g) =
        let ps' = filter (not . isPhi) (map simplify ps)
        in  PChoice ps' g
    simplify (PStar p g) = PStar (simplify p) g
    simplify (PPlus p1 p2) = PPlus (simplify p1) (simplify p2)
    simplify (PE r) = PE (map simplify r)


instance IsEpsilon Pat where
   isEpsilon (PVar _ _ p) = isEpsilon p
   isEpsilon (PE rs) = all isEpsilon rs                                                        
   isEpsilon (PPair p1 p2) =  (isEpsilon p1) && (isEpsilon p2)
   isEpsilon (PChoice ps _) =  all isEpsilon ps
   isEpsilon (PStar p _) = isEpsilon p
   isEpsilon (PPlus p1 p2) = isEpsilon p1 && isEpsilon p2
   isEpsilon (PEmpty _) = True
--                                                         

instance IsPhi Pat where
   isPhi (PVar _ _ p) = isPhi p
   isPhi (PE rs) = all isPhi rs                                                        
   isPhi (PPair p1 p2) =  (isPhi p1) || (isPhi p2)
   isPhi (PChoice ps _) =  all isPhi ps
   isPhi (PStar _ _) = False
   isPhi (PPlus p1 p2) = isPhi p1 || isPhi p2
   isPhi (PEmpty _) = False



