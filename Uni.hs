module Uni (increase) where

import Common

import UniUni ()
import Data.Generics.Uniplate.Direct

increase :: Integer -> Company -> Either String Company
increase = (runKureM Right Left .) . transformBiM . incS
