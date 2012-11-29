{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}

module SYB (reduce) where

import Common

import Data.Generics

import Control.Monad ((<=<), MonadPlus(..))

reduce :: Fib -> Int
reduce = (runKureM id error .) $ unLitRule <=< ev where



  ev :: GenericM KureM
  ev = fullBU (mkM $ tryR $ plusRule <+ evfibR)


  evfibR = fibBaseRule <+ (ev <=< fibStepRule)



(<+) f g = \a -> f a `mplus` g a
tryR x = x <+ return

instance MonadPlus KureM where
  mzero = fail "mzero"
  mplus = (<<+)



deriving instance Typeable Fib
deriving instance Data Fib



--full :: GenericM KureM -> GenericM KureM
--full = gmapM

--fullTD :: GenericM KureM -> GenericM KureM
--fullTD f = full (fullTD f) <=< f

fullBU :: GenericM KureM -> GenericM KureM
fullBU = everywhereM

