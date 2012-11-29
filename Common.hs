module Common (module Control.Monad, module Common) where

import Control.Monad ((<=<), liftM, ap)
import qualified Control.Monad


data KureM a = Failure String | Success a deriving (Eq, Show)

runKureM :: (a -> b) -> (String -> b) -> KureM a -> b
runKureM _ f (Failure msg) = f msg
runKureM s _ (Success a)   = s a



class Monad m => MonadCatch m where
  catchM :: m a -> (String -> m a) -> m a

(<<+) :: MonadCatch m => m a -> m a -> m a
ma <<+ mb = ma `catchM` const mb

(<+) f g = \a -> f a <<+ g a
tryR x = x <+ return

instance Monad KureM where
   return = Success

   Success a   >>= f = f a
   Failure msg >>= _ = Failure msg

   fail = Failure

instance MonadCatch KureM where
   Success a   `catchM` _ = Success a
   Failure msg `catchM` f = f msg


instance Control.Monad.MonadPlus KureM where
  mzero = fail "mzero"
  mplus = (<<+)





data Fib = Lit Int | Plus Fib Fib | Fib Fib deriving Show



-- semantics
eval :: Fib -> Int
eval (Lit i) = i
eval (Plus x y) = eval x + eval y
eval (Fib x) = case eval x of
  n -> case n of
    0 -> n
    1 -> n
    _ -> eval $ Fib (Lit $ n - 2) `Plus` Fib (Lit $ n - 1)


eval_old :: Fib -> Int
eval_old (Lit i) = i
eval_old (Plus x y) = eval x + eval y
eval_old (Fib (Lit 0)) = 0
eval_old (Fib (Lit 1)) = 1
eval_old (Fib n) = let n' = eval n in
  eval $ Fib (Lit $ n' - 2) `Plus` Fib (Lit $ n' - 1)



-- rules
plusRule :: Fib -> KureM Fib
plusRule (Plus (Lit x) (Lit y)) = return $ Lit $ x + y
plusRule _ = fail "plusRule"

fibBaseRule, fibStepRule :: Fib -> KureM Fib

fibBaseRule (Fib x@(Lit 0)) = return x
fibBaseRule (Fib x@(Lit 1)) = return x
fibBaseRule _ = fail "fibBaseRule"

fibStepRule (Fib n) = return $ Fib (n `Plus` Lit (0 - 2)) `Plus`
                               Fib (n `Plus` Lit (0 - 1))
fibStepRule _ = fail "fibStepRule"

unLitRule :: Fib -> KureM Int
unLitRule (Lit n) = return n
unLitRule _ = fail "unLitRule"
