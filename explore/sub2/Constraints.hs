{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Constraints where

import GHC.Generics (Generic)

import Unbound.Generics.LocallyNameless

import Types

-- | Equality constraint between two types.
data Eqn t where
  (:=:) :: t -> t -> Eqn t
  deriving (Eq, Show, Generic)

-- | Subtyping constraint between two types.
data Ineqn t where
  (:<:) :: t -> t -> Ineqn t
  deriving (Eq, Show, Generic)

-- | Class constraint.
data Qual t where
  Is :: Class -> t -> Qual t
  deriving (Eq, Show, Generic)

instance Alpha t   => Alpha (Eqn t)
instance Alpha t   => Alpha (Ineqn t)
instance Alpha t   => Alpha (Qual t)

instance Subst t t => Subst t (Eqn t)
instance Subst t t => Subst t (Ineqn t)
instance Subst t t => Subst t (Qual t)

toEqn :: Ineqn t -> Eqn t
toEqn (t1 :<: t2) = t1 :=: t2

-- | A constraint can be an equality, subtyping, or class constraint.
data Constraint t = CEqn (Eqn t) | CIneqn (Ineqn t) | CQual (Qual t)
  deriving (Eq, Show, Generic)

instance Alpha t => Alpha (Constraint t)
instance Subst t t => Subst t (Constraint t)

-- | Construct an equality constraint.
(===) :: t -> t -> Constraint t
t1 === t2 = CEqn (t1 :=: t2)

-- | Construct an inequality (subtyping) constraint constraint.
(=<=) :: t -> t -> Constraint t
t1 =<= t2 = CIneqn (t1 :<: t2)

-- | Construct a class constraint.
is :: Class -> t -> Constraint t
is c t = CQual (Is c t)

-- | Turn equations and inequations into equations.
getWeakEqn :: Constraint t -> Maybe (Eqn t)
getWeakEqn (CEqn e)             = Just e
getWeakEqn (CIneqn (t1 :<: t2)) = Just (t1 :=: t2)
getWeakEqn _                    = Nothing
