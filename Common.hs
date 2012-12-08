{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Common (module Common,
               module Language.KURE.Utilities,
               module Language.KURE.Combinators,
               module Control.Monad) where

import Language.KURE.Utilities (runKureM, KureM)
import Language.KURE.Combinators ((<<+))

import Control.Monad (liftM, liftM2, ap, (<=<))

import Test.QuickCheck

import Control.Applicative

import Control.DeepSeq (NFData)
import Control.DeepSeq.TH (deriveNFData)


newtype Company = C [Dept]
data Dept       = D Name Manager [Unit]
data Unit       = PU Employee | DU Dept
data Employee   = E Person Salary
data Person     = P Name Address
newtype Salary  = S Integer
type Manager    = Employee
type Name       = String
type Address    = String


deriving instance Show Company
deriving instance Show Dept
deriving instance Show Unit
deriving instance Show Employee
deriving instance Show Person
deriving instance Show Salary
deriving instance Eq Company
deriving instance Eq Dept
deriving instance Eq Unit
deriving instance Eq Employee
deriving instance Eq Person
deriving instance Eq Salary

{-# INLINE incS #-}
incS :: Monad m => Integer -> Salary -> m Salary
incS k = \(S x) -> return $ S $ x + k



instance Arbitrary Company where arbitrary = C <$> arbitrary
instance Arbitrary Dept where
  arbitrary = D <$> pure "dept" <*> arbitrary <*> arbitrary
instance Arbitrary Unit where
  arbitrary = frequency [(1000, PU <$> arbitrary), (1, DU <$> arbitrary)]
instance Arbitrary Employee where arbitrary = E <$> arbitrary <*> arbitrary
instance Arbitrary Person where arbitrary = P <$> pure "name" <*> pure "address"
instance Arbitrary Salary where arbitrary = S <$> arbitrary




deriving instance NFData Company
deriveNFData ''Dept
deriveNFData ''Unit
deriveNFData ''Employee
deriveNFData ''Person
deriving instance NFData Salary

