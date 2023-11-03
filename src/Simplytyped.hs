module Simplytyped
  ( conversion
  ,    -- conversion a terminos localmente sin nombre
    eval
  ,          -- evaluador
    infer
  ,         -- inferidor de tipos
    quote          -- valores -> terminos
  )
where

import           Data.List
import           Data.Maybe
import           Prelude                 hiding ( (>>=) )
import           Text.PrettyPrint.HughesPJ      ( render )
import           PrettyPrinter
import           Common

-- conversion a términos localmente sin nombres
conversion :: LamTerm -> Term
conversion = conversion' []

conversion' :: [String] -> LamTerm -> Term
conversion' b (LVar        n) = maybe (Free (Global n)) Bound (n `elemIndex` b)
conversion' b (LApp t u     ) = conversion' b t :@: conversion' b u
conversion' b (LAbs n t    u) = Lam t (conversion' (n : b) u)
--ej1
conversion' b (LLet s t1  t2) = Let (conversion' b t1) (conversion' (s:b) t2)
--ej2
conversion' _ LUnit           = Unit
--ej4
conversion' b (LPair t1   t2) = Pair (conversion' b t1) (conversion' b t2)
conversion' b (LFst        t) = Fst $ conversion' b t 
conversion' b (LSnd        t) = Snd $ conversion' b t
--ej5
conversion' _ LZero           = Zero
conversion' b (LSuc        t) = Suc $ conversion' b t
conversion' b (LRec t1 t2 t3) = Rec (conversion' b t1) (conversion' b t2) (conversion' b t3)
-----------------------
--- eval
-----------------------
-- i : Int -> t1 : Term -> t2 : Term -> Term
-- t2[i/t1] substituye las ocurrencias de i en t2 por t1 
sub :: Int -> Term -> Term -> Term
sub i t (Bound j) | i == j    = t
sub _ _ (Bound j) | otherwise = Bound j
sub _ _ (Free n   )           = Free n
sub i t (u   :@: v)           = sub i t u :@: sub i t v
sub i t (Lam t'  u)           = Lam t' (sub (i + 1) t u)
--ej1
sub i t (Let t1 t2)           = Let (sub i t t1) (sub (i + 1) t t2)
--ej2
sub _ _ Unit                  = Unit
--ej4
sub i t (Pair t1 t2)          = Pair (sub i t t1) (sub i t t2)
sub i t (Fst t')              = Fst $ sub i t t'
sub i t (Snd t')              = Snd $ sub i t t'
--ej5
sub _ _ Zero                  = Zero
sub i t (Suc t')              = Suc $ sub i t t'
sub i t (Rec t1 t2 t3)        = Rec (sub i t t1) (sub i t t2) (sub i t t3)


-- evaluador de términos
eval :: NameEnv Value Type -> Term -> Value
eval _ (Bound _             ) = error "variable ligada inesperada en eval"
eval e (Free  n             ) = fst $ fromJust $ lookup n e
eval _ (Lam      t   u      ) = VLam t u
eval e (Lam _ u  :@: Lam s v) = eval e (sub 0 (Lam s v) u)
eval e (Lam t u1 :@: u2     ) = let v2 = eval e u2 
                                in eval e (sub 0 (quote v2) u1)
eval e (u        :@: v      ) = case eval e u of
  VLam t u' -> eval e (Lam t u' :@: v)
  _         -> error "Error de tipo en run-time, verificar type checker"
--ej1
eval e (Let t1 t2) = 
  let t1' = eval e t1 in eval e (sub 0 (quote t1') t2)
--ej2
eval _ Unit = VUnit
--ej4
eval e (Pair t1 t2) = VPair (eval e t1) (eval e t2)
eval e (Fst t) = case eval e t of
                   VPair v1 _ -> v1
                   _          -> error "Error de tipo en run-time, verificar type checker"
eval e (Snd t) = case eval e t of
                   VPair _ v2 -> v2
                   _          -> error "Error de tipo en run-time, verificar type checker"
--ej5
eval e Zero    = VNum NZero
eval e (Suc t) = case eval e t of
                   VNum n -> VNum $ NSuc n
                   _      -> error "Error de tipo en run-time, verificar type checker"
eval e (Rec t u v)    = 
  case eval e v of
    VNum NZero     -> eval e t
    VNum (NSuc nv) -> let n = quotenv nv
                          r = quote $ eval e (Rec t u n)
                      in eval e ((u :@: r) :@: n) 
-----------------------
--- quoting
-----------------------

quote :: Value -> Term
quote (VLam t      f) = Lam t f
quote VUnit           = Unit
quote (VPair t1   t2) = Pair (quote t1) (quote t2)
quote (VNum    NZero) = Zero
quote (VNum (NSuc n)) = Suc $ quotenv n 

quotenv :: NumVal -> Term
quotenv NZero    = Zero
quotenv (NSuc n) = Suc $ quotenv n

----------------------
--- type checker
-----------------------

-- type checker
infer :: NameEnv Value Type -> Term -> Either String Type
infer = infer' []

-- definiciones auxiliares
ret :: Type -> Either String Type
ret = Right

err :: String -> Either String Type
err = Left

(>>=)
  :: Either String Type -> (Type -> Either String Type) -> Either String Type
(>>=) v f = either Left f v
-- fcs. de error

matchError :: Type -> Type -> Either String Type
matchError t1 t2 =
  err
    $  "se esperaba "
    ++ render (printType t1)
    ++ ", pero "
    ++ render (printType t2)
    ++ " fue inferido."

notfunError :: Type -> Either String Type
notfunError t1 = err $ render (printType t1) ++ " no puede ser aplicado."

notfoundError :: Name -> Either String Type
notfoundError n = err $ show n ++ " no está definida."

notpairError :: Type -> Either String Type
notpairError termT = err $ render (printType termT) ++ " fst y snd admiten solamente pares. "

-- data Name = Global String
-- type NameEnv v t = [(Name, (v, t))]
infer' :: Context -> NameEnv Value Type -> Term -> Either String Type
infer' c _ (Bound i) = ret (c !! i)
infer' _ e (Free  n) = case lookup n e of
  Nothing     -> notfoundError n
  Just (_, t) -> ret t
infer' c e (t :@: u) = infer' c e t >>= \tt -> infer' c e u >>= \tu ->
  case tt of
    FunT t1 t2 -> if (tu == t1) then ret t2 else matchError t1 tu
    _          -> notfunError tt
infer' c e (Lam t u) = infer' (t : c) e u >>= \tu -> ret $ FunT t tu
--ej1
infer' c e (Let t u) = infer' c e t >>= \tt -> infer' (tt:c) e u
--ej2
infer' c e Unit = ret UnitT
--ej4
infer' c e (Pair t u) = infer' c e t >>= \tt -> infer' c e u
                        >>= \tu -> ret $ PairT tt tu
infer' c e (Fst t)    = infer' c e t >>= \tt -> 
  case tt of
    PairT tt1 _ -> ret tt1
    termT       -> notpairError termT
infer' c e (Snd t)    = infer' c e t >>= \tt -> 
  case tt of
    PairT _ tt2 -> ret tt2
    termT       -> notpairError termT
--ej5
infer' _ _ Zero        = ret NatT
infer' c e (Suc n)     = infer' c e n >>= \tn -> 
  case tn of
    NatT  -> ret NatT
    termT -> matchError NatT termT
infer' c e (Rec t u v) = infer' c e t >>= \tt -> infer' c e u
                         >>= \tu -> infer' c e v >>= \tv ->
  if tu /= (FunT tt (FunT NatT tt))
  then matchError (FunT tt (FunT NatT tt)) tu
  else if tv /= NatT
  then matchError NatT tv
  else ret tt
----------------------------------
