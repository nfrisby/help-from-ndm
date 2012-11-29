module Hand (reduce) where

import Common

import Control.Monad ((<=<), liftM, ap)



reduce :: Fib -> Int
reduce = (runKureM id error .) $ unLitRule <=< ev where




  ev = allbuR (tryR $ plusRule <+ evfibR)


  evfibR = fibBaseRule <+ (ev <=< fibStepRule)



(<+) f g = \a -> f a <<+ g a
tryR x = x <+ return



allR :: (Fib -> KureM Fib) -> Fib -> KureM Fib
allR f (Plus a b) = Plus `liftM` f a `ap` f b
allR f (Fib a) = Fib `liftM` f a
allR _ x = return x

allbuR :: (Fib -> KureM Fib) -> Fib -> KureM Fib
allbuR f = let go = f <=< allR go in go
