{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}

module SYB (reduce) where

import Common

import Data.Generics



reduce :: Fib -> Int
reduce = (runKureM id error .) $ unLitRule <=< ev where



  ev :: GenericM KureM
  ev = fullBU (mkM $ tryR $ plusRule <+ evfibR)


  evfibR = fibBaseRule <+ (ev <=< fibStepRule)



deriving instance Typeable Fib
deriving instance Data Fib



fullBU :: GenericM KureM -> GenericM KureM
fullBU = everywhereM

{- static-argument transformation would benefit everywhereM
{-# INLINE everywhereM' #-}
everywhereM' :: GenericM KureM -> GenericM KureM
everywhereM' f = let go :: Generic KureM
                     go x = gmapM go x >>= f
                 in go
-}
