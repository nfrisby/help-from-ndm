{-# LANGUAGE MultiParamTypeClasses #-}

module UniUni () where

import Common

import Data.Generics.Uniplate.Direct

instance Biplate Company Salary where
  biplate (C x1) = plate C ||+ x1

instance Biplate Dept Salary where
  biplate (D x1 x2 x3) = plate (D x1) |+ x2 ||+ x3

instance Biplate Employee Salary where
  biplate (E x1 x2) = plate (E x1) |* x2

instance Biplate Unit Salary where
  biplate (PU x1) = plate PU |+ x1
  biplate (DU x1) = plate DU |+ x1

instance Biplate Person Salary where
  biplate x = plate x

instance Biplate Salary Salary where
  biplate = plateSelf

instance Uniplate Salary where
  uniplate = plate
