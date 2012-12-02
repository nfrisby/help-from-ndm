module Hand (reduce) where

import Common



reduce :: Fib -> Int
reduce = (runKureM id error .) $ unLitRule <=< ev where




  ev = allbuR (tryR $ plusRule <+ evfibR)


  evfibR = fibBaseRule <+ (ev <=< fibStepRule)




allR :: (Fib -> KureM Fib) -> Fib -> KureM Fib
allR f (Plus a b) = liftM2 Plus (f a) (f b)
allR f (Fib a) = Fib `liftM` f a
allR _ x = return x

allbuR :: (Fib -> KureM Fib) -> Fib -> KureM Fib
allbuR f = let go = f <=< allR go in go
