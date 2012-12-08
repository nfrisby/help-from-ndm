{-# LANGUAGE TemplateHaskell #-}

module TYB (increase) where

import Common

import Data.Generics.TH

increase :: Integer -> Company -> Either String Company
increase k = (runKureM Right Left .) $ $(everywhereForM 'w [t|Company|])
  where w = incS k
