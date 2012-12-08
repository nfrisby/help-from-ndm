{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}

module SYBAux (module SYBAux, module Data.Generics) where

import Data.Generics

everywhereMBut :: forall m. Monad m =>
  GenericQ Bool -> GenericM m -> GenericM m
everywhereMBut q f = w where
  w :: GenericM m
  w x | q x = return x
      | otherwise = gmapM w x >>= f
