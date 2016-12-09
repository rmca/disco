{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE UndecidableInstances  #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Disco.Types
-- Copyright   :  (c) 2016 disco team (see LICENSE)
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  byorgey@gmail.com
--
-- Disco language types.
--
-----------------------------------------------------------------------------

module Disco.Types
       ( Type, MonoType(..), mono

       , isNumTy, Strictness(..), strictness
       )
       where

import           Unbound.LocallyNameless
import           Unbound.LocallyNameless.Ops   (unsafeUnbind)
import           Unbound.LocallyNameless.Types (SetBind)

-- | A type is a monotype together with an optional set of universally
--   quantified type variables.
type Type = SetBind [Name MonoType] MonoType

-- | Monotypes.
data MonoType where
  -- | Type variables (for polymorphism)
  TyVar    :: Name MonoType -> MonoType

    -- If we later switch to a unification-based system, we will need
    -- another constructor for unification variables.  Ideally Type
    -- would be parameterized by a variable type, but I can't figure out
    -- how to make that work with unbound.

  -- | The void type, with no inhabitants.
  TyVoid   :: MonoType

  -- | The unit type, with one inhabitant.
  TyUnit   :: MonoType

  -- | Booleans.
  TyBool   :: MonoType

  -- | Function type, T1 -> T2
  TyArr    :: MonoType -> MonoType -> MonoType

  -- | Pair type, T1 * T2
  TyPair   :: MonoType -> MonoType -> MonoType

  -- | Sum type, T1 + T2
  TySum    :: MonoType -> MonoType -> MonoType

  -- | Natural numbers
  TyN      :: MonoType

  -- | Integers
  TyZ      :: MonoType

  -- | Rationals
  TyQ      :: MonoType

  -- | Lists
  TyList   :: MonoType -> MonoType

  deriving (Show, Eq)

-- | Construct a monotype with no quantification.
mono :: MonoType -> Type
mono m = permbind [] m

-- | Check whether a type is a numeric type (N, Z, or Q).
isNumTy :: Type -> Bool
isNumTy ty = snd (unsafeUnbind ty) `elem` [TyN, TyZ, TyQ]

-- | Strictness of a function application or let-expression.
data Strictness = Strict | Lazy
  deriving (Eq, Show)

-- | Numeric types are strict, others are lazy.
strictness :: Type -> Strictness
strictness ty
  | isNumTy ty = Strict
  | otherwise  = Lazy

derive [''MonoType, ''Strictness]

instance Alpha MonoType
instance Alpha Strictness

