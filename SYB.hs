module SYB (increase, stopper) where

import Common

import SYBSYB ()
import SYBAux

increase :: Integer -> Company -> Either String Company
increase k = (runKureM Right Left .) $ everywhereMBut stopper $ mkM $ incS k

stopper :: GenericQ Bool
stopper = mkQ False (\P{} -> True) `extQ` (\x -> let _ = x `asTypeOf` "" in True)
