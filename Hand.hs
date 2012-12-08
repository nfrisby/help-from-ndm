module Hand (increase) where

import Common

increase :: Integer -> Company -> Either String Company
increase k = (runKureM Right Left .) $ allbuR_C_S (incS k) where
  allbuR_C_S f (C ds) = C `liftM` mapM (allbuR_D_S f) ds
  allbuR_D_S f (D n m us) =
    D n `liftM` allbuR_E_S f m `ap` mapM (allbuR_U_S f) us
  allbuR_E_S f (E p s) = E p `liftM` f s
  allbuR_U_S f (PU e) = PU `liftM` allbuR_E_S f e
  allbuR_U_S f (DU d) = DU `liftM` allbuR_D_S f d
