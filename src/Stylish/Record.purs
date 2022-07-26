module Stylish.Record where

import Prelude

import Control.Apply (lift2)
import Data.Foldable (class Foldable, foldMap)
import Data.Functor.App (App(..))
import Data.Identity (Identity(..))
import Data.List (List(..))
import Data.Newtype (un, wrap)
import Data.Symbol (class IsSymbol, reflectSymbol)
import Prim.Row as Row
import Prim.RowList (class RowToList, RowList, Nil, Cons)
import Record as Record
import Stylish.Types (Classy(..), Stylish(..))
import Type.Proxy (Proxy(..))

class WithSep o where
  withoutSep :: String -> o
  withSep :: String -> o -> o

instance withSepStylish :: WithSep Stylish where
  withoutSep = wrap
  withSep k (Stylish v) = Stylish (k <> ":" <> v)
instance withSepClassy :: WithSep Classy where
  withoutSep = wrap
  withSep k (Classy v) = Classy (k <> "-" <> v)

withSeps :: forall o. WithSep o => List String -> o -> o
withSeps Nil = identity
withSeps (Cons s r) = withSeps r <<< withSep s

withSeps1 :: forall o. WithSep o => List String -> o
withSeps1 Nil = withoutSep ""
withSeps1 (Cons s r) = withoutSep s # withSeps r

attrs :: forall i o. Attrable i Identity o => i -> o
attrs = un Identity <<< attrsM

attrsM :: forall i m o. Attrable i m o => i -> m o
attrsM = mkAttr Nil

class Attrable i (m :: Type -> Type) o where
  mkAttr :: List String -> i -> m o

instance attrableUnit :: (WithSep o, Applicative m) => Attrable Unit m o where
  mkAttr k (_ :: Unit) = pure (withSeps1 k)

instance attrableBoolean :: (Monoid o, WithSep o, Applicative m) => Attrable Boolean m o where
  mkAttr k = if _
    then mkAttr k unit
    else pure mempty

instance attrableWithStep :: (WithSep o, Applicative m) => Attrable String m o where
  mkAttr k v = pure (withSeps k (withoutSep v))

instance attrableStylish :: (Applicative m) => Attrable Stylish m Stylish where
  mkAttr k v = pure (withSeps k v)

instance attrableClassy :: (Applicative m) => Attrable Classy m Classy where
  mkAttr k v = pure (withSeps k v)

instance attrableRecord :: (RowToList is irl, AttrableRecord is irl m o) => Attrable (Record is) m o where
  mkAttr k = mkAttrRecord (Proxy :: Proxy irl) k

else instance attrableFunctor :: (Functor m, Attrable i Identity o) => Attrable (m i) m o where
  mkAttr k mi = mi <#> un Identity <<< mkAttr k

else instance attrableFoldable :: (Foldable f, Applicative m, Monoid o, Attrable i m o) => Attrable (f i) m o where
  mkAttr k = un App <<< foldMap (App <<< mkAttr k)

class AttrableRecord :: Row Type -> RowList Type -> (Type -> Type) -> Type -> Constraint
class AttrableRecord is irl m o where
  mkAttrRecord :: Proxy irl -> List String -> Record is -> m o

instance attrableRecordNil :: (Applicative m, Monoid o) => AttrableRecord is Nil m o where
  mkAttrRecord _ _ _ = pure mempty

instance attrableRecordCons :: (IsSymbol k, Row.Cons k i is' is, AttrableRecord is irl m o, Attrable i m o, Applicative m, Monoid o) => AttrableRecord is (Cons k i irl) m o where
  mkAttrRecord _ ks is = lift2 append
    do
      mkAttr (Cons (reflectSymbol (Proxy :: Proxy k)) ks) (Record.get (Proxy :: Proxy k) is :: i)
    do
      mkAttrRecord (Proxy :: Proxy irl) ks is
