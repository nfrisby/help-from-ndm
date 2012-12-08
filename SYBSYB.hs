{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}

module SYBSYB () where

import Common

import SYBAux

deriving instance Data Company
deriving instance Typeable Company
deriving instance Data Dept
deriving instance Typeable Dept
deriving instance Data Unit
deriving instance Typeable Unit
deriving instance Data Employee
deriving instance Typeable Employee
deriving instance Data Person
deriving instance Typeable Person
deriving instance Data Salary
deriving instance Typeable Salary
