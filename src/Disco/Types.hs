{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}

module Disco.Types where

import           Generics.MultiRec
import           Generics.MultiRec.TH

data Name a = Free String | Bound Int
  deriving (Show, Functor)

-- | A program is a list of declarations.
type Prog = [Decl]

-- | A declaration is either a type declaration or (one clause of a) definition.
data Decl where
  DType :: Name Term -> Type -> Decl
  DDefn :: Name Term -> ([Pattern], Term) -> Decl
  deriving Show

-- | Injections into a sum type (inl or inr) have a "side" (L or R).
data Side = LS | RS
  deriving (Show, Eq, Enum)

-- | Unary operators.
data UOp = Neg | Not
  deriving (Show, Eq)

-- | Binary operators.
data BOp = Add | Sub | Mul | Div | Exp | Eq | Neq | Lt | Gt | Leq | Geq | And | Or | Mod
         | Divides | RelPm
  deriving (Show, Eq)

-- XXX todo add TRat with ability to parse decimal notation

-- | Terms.
data Term where
  TVar   :: Name Term -> Term                  -- ^ Variable
  TUnit  :: Term                               -- ^ Unit ()
  TBool  :: Bool -> Term                       -- ^ Boolean
  TAbs   :: (Name Term, Term) -> Term      -- ^ Anonymous function abstraction
  TJuxt  :: Term -> Term -> Term               -- ^ Juxtaposition (can be either
                                               --   function application or multiplication)
  TPair  :: Term -> Term -> Term               -- ^ Ordered pairs (x,y)
  TInj   :: Side -> Term -> Term               -- ^ Injection into a sum type
  TNat   :: Integer -> Term                    -- ^ A natural number
  TUn    :: UOp -> Term -> Term                -- ^ Application of a unary operator
  TBin   :: BOp -> Term -> Term -> Term        -- ^ Application of a binary operator
  TLet   :: ((Name Term, Term), Term) -> Term
                                               -- ^ Non-recursive let expression
                                               --   (let x = t1 in t2)
  TCase  :: [Branch] -> Term                   -- ^ A case expression
                                               --   consists of a list
                                               --   of branches.
  TAscr  :: Term -> Type -> Term               -- ^ Type ascription (expr : type)
  deriving Show

-- | A branch of a case is a list of guards with an accompanying term.
--   The guards scope over the term.  Additionally, each guard scopes
--   over subsequent guards.
type Branch = (Guards, Term)

data Guards where
  GEmpty :: Guards
  GCons  :: (Guard, Guards) -> Guards
  deriving Show

-- | A single guard in a branch.
data Guard where
  GIf   :: Term -> Guard             -- ^ Boolean guard (if <test>)
  GWhen :: Term -> Pattern -> Guard  -- ^ Pattern guard (when term = pat)
  deriving Show

-- | Patterns.
data Pattern where
  PVar  :: Name Term -> Pattern             -- ^ Variable
  PWild :: Pattern                          -- ^ Wildcard _
  PUnit :: Pattern                          -- ^ Unit ()
  PBool :: Bool -> Pattern                  -- ^ Literal boolean
  PPair :: Pattern -> Pattern -> Pattern    -- ^ Pair pattern (pat1, pat2)
  PInj  :: Side -> Pattern -> Pattern       -- ^ Injection pattern (inl pat or inr pat)
  PNat  :: Integer -> Pattern               -- ^ Literal natural number pattern
  PSucc :: Pattern -> Pattern               -- ^ Successor pattern, (succ n)
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

-- derive [''Side, ''UOp, ''BOp, ''Term, ''Guards, ''Guard, ''Pattern, ''Type]

-- instance Alpha Side
-- instance Alpha UOp
-- instance Alpha BOp
-- instance Alpha Term
-- instance Alpha Guards
-- instance Alpha Guard
-- instance Alpha Pattern
-- instance Alpha Type

-- instance Subst Term Type
-- instance Subst Term Guards
-- instance Subst Term Guard
-- instance Subst Term Pattern
-- instance Subst Term Side
-- instance Subst Term BOp
-- instance Subst Term UOp
-- instance Subst Term Term where
--   isvar (TVar x) = Just (SubstName x)
--   isvar _ = Nothing

isNumTy :: Type -> Bool
isNumTy ty = ty `elem` [TyN, TyZ, TyQ]

data AST f where
  Prog    :: AST Prog
  Decl    :: AST Decl
  Term    :: AST Term
  Branch  :: AST Branch
  Guards  :: AST Guards
  Guard   :: AST Guard
  Pattern :: AST Pattern
  Type    :: AST Type

$(deriveAll ''AST)
