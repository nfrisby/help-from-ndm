module Uni (reduce) where

import Data.Generics.Uniplate.Direct

import Common







reduce :: Fib -> Int
reduce = (runKureM id error .) $ unLitRule <=< ev where




  ev = fullBU (tryR $ plusRule <+ evfibR)


  evfibR = fibBaseRule <+ (ev <=< fibStepRule)



instance Uniplate Fib where
  --  {-# INLINE uniplate #-} -- worsens performance
  uniplate (Plus a b) = plate Plus |* a |* b
  uniplate (Fib a) = plate Fib |* a
  uniplate x = plate x

{- -- vastly improves performance
  descendM f (Plus a b) = Plus `liftM` f a `ap` f b
  descendM f (Fib a) = Fib `liftM` f a
  descendM _ x = return x
-}

fullBU :: (Fib -> KureM Fib) -> Fib -> KureM Fib
fullBU = transformM
