{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE UndecidableInstances  #-}

module Disco.Types where

import           Unbound.LocallyNameless

-- | A program is a list of declarations.
type Prog x = (x, [Decl x])

type Decl x = (x, Decl' x)

-- | A declaration is either a type declaration or (one clause of a) definition.
data Decl' x where
  DType :: Name (Term x) -> Type -> Decl' x
  DDefn :: Name (Term x) -> Bind [Pattern x] (Term x) -> Decl' x
  deriving Show

-- | Injections into a sum type (inl or inr) have a "side" (L or R).
data Side = L | R
  deriving (Show, Eq, Enum)

-- | Unary operators.
data UOp = Neg | Not
  deriving (Show, Eq)

-- | Binary operators.
data BOp = Add | Sub | Mul | Div | Exp | Eq | Neq | Lt | Gt | Leq | Geq | And | Or | Mod
         | Divides | RelPm
  deriving (Show, Eq)

-- XXX todo add TRat with ability to parse decimal notation

type Term x = (x, Term' x)

-- | Terms.
data Term' x where
  TVar   :: Name (Term x) -> Term' x                  -- ^ Variable
  TUnit  :: Term' x                               -- ^ Unit ()
  TBool  :: Bool -> Term' x                       -- ^ Boolean
  TAbs   :: Bind (Name (Term x)) (Term x) -> Term' x      -- ^ Anonymous function abstraction
  TJuxt  :: Term x -> Term x -> Term' x               -- ^ Juxtaposition (can be either
                                               --   function application or multiplication)
  TPair  :: Term x -> Term x -> Term' x               -- ^ Ordered pairs (x,y)
  TInj   :: Side -> Term x -> Term' x               -- ^ Injection into a sum type
  TNat   :: Integer -> Term' x                    -- ^ A natural number
  TUn    :: UOp -> Term x -> Term' x                -- ^ Application of a unary operator
  TBin   :: BOp -> Term x -> Term x -> Term' x        -- ^ Application of a binary operator
  TLet   :: Bind (Name (Term x), Embed (Term x)) (Term x) -> Term' x
                                               -- ^ Non-recursive let expression
                                               --   (let x = t1 in t2)
  TCase  :: [Branch x] -> Term' x                   -- ^ A case expression
                                               --   consists of a list
                                               --   of branches.
  TAscr  :: Term x -> Type -> Term' x               -- ^ Type ascription (expr : type)
  deriving Show

-- | A branch of a case is a list of guards with an accompanying term.
--   The guards scope over the term.  Additionally, each guard scopes
--   over subsequent guards.
type Branch x = Bind (Guards x) (Term x)

data Guards x where
  GEmpty :: Guards x
  GCons  :: Rebind (Guard x) (Guards x) -> Guards x
  deriving Show

-- | A single guard in a branch.
data Guard x where
  GIf   :: Embed (Term x) -> Guard x             -- ^ Boolean guard (if <test>)
  GWhen :: Embed (Term x) -> (Pattern x) -> Guard x  -- ^ Pattern guard (when term = pat)
  deriving Show

type Pattern x = (x, Pattern' x)

-- | Patterns.
data Pattern' x where
  PVar  :: Name (Term x) -> Pattern' x             -- ^ Variable
  PWild :: Pattern' x                          -- ^ Wildcard _
  PUnit :: Pattern' x                         -- ^ Unit ()
  PBool :: Bool -> Pattern' x                  -- ^ Literal boolean
  PPair :: Pattern x -> Pattern x -> Pattern' x    -- ^ Pair pattern (pat1, pat2)
  PInj  :: Side -> Pattern x -> Pattern' x       -- ^ Injection pattern (inl pat or inr pat)
  PNat  :: Integer -> Pattern' x               -- ^ Literal natural number pattern
  PSucc :: Pattern x -> Pattern' x               -- ^ Successor pattern, (succ n)
  deriving Show
  -- TODO: figure out how to match on Z or Q!

-- | Types.
data Type where
  TyVoid   :: Type                  -- ^ Void
  TyUnit   :: Type                  -- ^ Unit
  TyBool   :: Type                  -- ^ Bool
  TyArr    :: Type -> Type -> Type  -- ^ Function type,  T1 -> T2
  TyPair   :: Type -> Type -> Type  -- ^ Pair type, T1 * T2
  TySum    :: Type -> Type -> Type  -- ^ Sum type, T1 + T2
  TyN      :: Type                  -- ^ Natural numbers
  TyZ      :: Type                  -- ^ Integers
  TyQ      :: Type                  -- ^ Rationals
  deriving (Show, Eq)

derive [''Side, ''UOp, ''BOp, ''Term', ''Guards, ''Guard, ''Pattern', ''Type]

instance Alpha Side
instance Alpha UOp
instance Alpha BOp
instance Alpha (Term' x)
instance Alpha (Guards x)
instance Alpha (Guard x)
instance Alpha (Pattern' x)
instance Alpha Type

instance Subst (Term' x) Type
instance Subst (Term' x) (Guards x)
instance Subst (Term' x) (Guard x)
instance Subst (Term' x) (Pattern' x)
instance Subst (Term' x) Side
instance Subst (Term' x) BOp
instance Subst (Term' x) UOp
instance Subst (Term' x) (Term' x) where
  isvar (TVar x) = Just (SubstName x)
  isvar _ = Nothing

isNumTy :: Type -> Bool
isNumTy ty = ty `elem` [TyN, TyZ, TyQ]
