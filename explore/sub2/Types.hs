{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms       #-}

{-# OPTIONS_GHC -fno-warn-orphans  #-}

module Types where

import           Data.Coerce  (coerce)
import           GHC.Generics (Generic)

import           Data.List    (nub)
import           Data.Map     (Map)
import qualified Data.Map     as M
import           Data.Monoid
import           Data.Set     (Set)
import qualified Data.Set     as S

import           Control.Arrow ((***))
import           Control.Lens (toListOf)

import           Unbound.Generics.LocallyNameless

import           Subst

------------------------------------------------------------
-- Atomic types
------------------------------------------------------------

data Atom where
  AVar :: Name Type -> Sort -> Atom
  ANat :: Atom
  AInt :: Atom
  deriving (Show, Eq, Ord, Generic)

newtype Opaque a = Opaque { getOpaque :: a }
  deriving (Eq, Ord, Show)

instance (Show a, Ord a) => Alpha (Opaque a) where
  aeq'        _        = (==)
  fvAny'      _ _      = pure
  close       _ _      = id
  open        _ _      = id
  isPat       _        = mempty
  isTerm      _        = mempty
  nthPatFind  _        = mempty
  namePatFind _        = mempty
  swaps'      _ _      = id
  freshen'    _ i      = return (i,mempty)
  lfreshen'   _ i cont = cont i mempty
  acompare'   _        = compare

instance Subst t (Opaque a) where
  isvar _ = Nothing
  subst _ _ = id
  substs _ = id

instance Alpha Atom

instance Subst Atom Atom where
  isvar (AVar x _) = Just (SubstName (coerce x))
  isvar _          = Nothing

isVar :: Atom -> Bool
isVar (AVar {}) = True
isVar _         = False

isBase :: Atom -> Bool
isBase = not . isVar

isSub :: Atom -> Atom -> Bool
isSub a1 a2 | a1 == a2 = True
isSub ANat AInt = True
isSub _ _ = False

ainf2 :: Atom -> Atom -> Maybe Atom
ainf2 a1 a2     | a1 == a2 = Just a1
ainf2 AInt ANat = Just ANat
ainf2 ANat AInt = Just ANat
ainf2 _ _       = Nothing

ainf :: [Atom] -> Maybe Atom
ainf []  = Nothing
ainf [a] = Just a
ainf (a:as) = do
  g <- ainf as
  ainf2 a g

asup2 :: Atom -> Atom -> Maybe Atom
asup2 a1 a2     | a1 == a2 = Just a1
asup2 AInt ANat = Just AInt
asup2 ANat AInt = Just AInt
asup2 _ _       = Nothing

asup :: [Atom] -> Maybe Atom
asup []  = Nothing
asup [a] = Just a
asup (a:as) = do
  g <- asup as
  asup2 a g

------------------------------------------------------------
-- Type structure
------------------------------------------------------------

data Cons where
  CArr  :: Cons
  CPair :: Cons
  deriving (Show, Eq, Ord, Generic)

instance Alpha Cons

data Variance = Co | Contra

arity :: Cons -> [Variance]
arity CArr  = [Contra, Co]
arity CPair = [Co, Co]

------------------------------------------------------------
-- Types
------------------------------------------------------------

data Type where
  TyAtom :: Atom -> Type
  TyCons :: Cons -> [Type] -> Type
  deriving (Show, Eq, Ord, Generic)

instance Alpha Type

instance Subst Type Atom
instance Subst Type Cons
instance Subst Type Type where
  isvar (TyAtom (AVar x _)) = Just (SubstName x)
  isvar _                   = Nothing

-- orphans
instance (Ord a, Subst t a) => Subst t (Set a) where
  subst x t = S.map (subst x t)
  substs s  = S.map (substs s)
instance (Ord k, Subst t a) => Subst t (Map k a) where
  subst x t = M.map (subst x t)
  substs s  = M.map (substs s)

type S = S' Type

atomToTypeSubst :: S' Atom -> S' Type
atomToTypeSubst = map (coerce *** TyAtom)

------------------------------------------------------------
-- Convenience
------------------------------------------------------------

var :: String -> Type
var x = TyVar (string2Name x) top

freshTy :: Fresh m => m Type
freshTy = TyVar <$> fresh (string2Name "a") <*> pure top

pattern TyVar :: Name Type -> Sort -> Type
pattern TyVar v s = TyAtom (AVar v s)

pattern TyNat :: Type
pattern TyNat   = TyAtom ANat

pattern TyInt :: Type
pattern TyInt   = TyAtom AInt

pattern TyFun :: Type -> Type -> Type
pattern TyFun ty1 ty2 = TyCons CArr [ty1, ty2]

pattern TyPair :: Type -> Type -> Type
pattern TyPair ty1 ty2 = TyCons CPair [ty1, ty2]

------------------------------------------------------------
-- Classes
------------------------------------------------------------

data Class where
  Numeric  :: Class
  Negative :: Class
  deriving (Eq, Ord, Show, Generic)

instance Alpha Class
instance Subst t Class

type Sort = Opaque (Set Class)

member :: Class -> Sort -> Bool
member c (Opaque s) = c `S.member` s

union :: Sort -> Sort -> Sort
union (Opaque s1) (Opaque s2) = Opaque (s1 `S.union` s2)

top :: Sort
top = Opaque S.empty

-- Given a type constructor application C ty1 ... tyn, what sorts are
-- necessary for each of the types ty1, ..., tyn in order for the
-- application C ty1 .. tyn to have a given sort?
data SortArity
  = Impossible
  | ArgSorts [Sort]

instance Monoid SortArity where
  mempty = ArgSorts []
  Impossible `mappend` _ = Impossible
  _ `mappend` Impossible = Impossible
  ArgSorts s1 `mappend` ArgSorts s2 = ArgSorts (zipWith union s1 s2)

classArity :: Cons -> Class -> SortArity
classArity CArr Numeric  = Impossible -- Just [S.empty, S.fromList [Numeric]]
classArity CArr Negative = Impossible -- Just [S.empty, S.fromList [Negative]]

classArity CPair Numeric  = Impossible -- for now
classArity CPair Negative = Impossible

sortArity :: Cons -> Sort -> SortArity
sortArity c = foldMap (classArity c) . getOpaque

atomInClass :: Class -> Atom -> Bool
atomInClass c (AVar _ (Opaque s)) = S.member c s
atomInClass Numeric  a            = a `elem` [ANat, AInt]
atomInClass Negative a            = (a == AInt)

inClass :: Class -> Type -> Bool
inClass cl (TyAtom a)     = atomInClass cl a
inClass cl (TyCons c tys) =
  case classArity c cl of
    Impossible  -> False
    ArgSorts ss -> and $ zipWith inSort ss tys

inSort :: Sort -> Type -> Bool
inSort (Opaque s) ty = getAll $ foldMap (All . flip inClass ty) s

------------------------------------------------------------
-- Sigma types
------------------------------------------------------------

data Sigma where
  Forall :: Bind [Name Type] Type -> Sigma
  deriving (Show, Generic)

instance Alpha Sigma
instance Subst Type Sigma

generalize :: Type -> Sigma
generalize ty = Forall (bind newvs (substs newvsubst ty))
  where
    fvlist = nub $ toListOf fv ty
    newvs  = take (length fvlist) $ map (string2Name . (:[])) ['a' .. 'z']
    newvsubst = zip fvlist (map (\n -> TyVar n top) newvs)
