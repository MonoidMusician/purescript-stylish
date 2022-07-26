module Stylish.Types where

import Prelude

import Data.Newtype (class Newtype, over2)

appendSep :: forall m. Eq m => Monoid m => m -> m -> m -> m
appendSep sep l r
  | l == mempty = r
  | r == mempty = l
  | otherwise = l <> sep <> r

newtype Stylish = Stylish String
derive newtype instance eqStylish :: Eq Stylish
derive newtype instance ordStylish :: Ord Stylish

instance showStylish :: Show Stylish where
  show (Stylish s) = s

derive instance newtypeStylish :: Newtype Stylish _

instance semigroupStylish :: Semigroup Stylish where
  append = over2 Stylish (appendSep ";")
instance monoidStylish :: Monoid Stylish where
  mempty = Stylish mempty

newtype Classy = Classy String
derive newtype instance eqClassy :: Eq Classy
derive newtype instance ordClassy :: Ord Classy

instance showClassy :: Show Classy where
  show (Classy s) = s

derive instance newtypeClassy :: Newtype Classy _

instance semigroupClassy :: Semigroup Classy where
  append = over2 Classy (appendSep " ")
instance monoidClassy :: Monoid Classy where
  mempty = Classy mempty
