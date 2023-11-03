module PrettyPrinter
  ( printTerm
  ,     -- pretty printer para terminos
    printType     -- pretty printer para tipos
  )
where

import           Common
import           Text.PrettyPrint.HughesPJ
import           Prelude                 hiding ( (<>) )
-- lista de posibles nombres para variables
vars :: [String]
vars =
  [ c : n
  | n <- "" : map show [(1 :: Integer) ..]
  , c <- ['x', 'y', 'z'] ++ ['a' .. 'w']
  ]

parensIf :: Bool -> Doc -> Doc
parensIf True  = parens
parensIf False = id

-- pretty-printer de términos

pp :: Int -> [String] -> Term -> Doc
pp ii vs (Bound k         ) = text (vs !! (ii - k - 1))
pp _  _  (Free  (Global s)) = text s

pp ii vs (i :@: c         ) = sep
  [ parensIf (isLam i) (pp ii vs i)
  , nest 1 (parensIf (isNoTerminal c) (pp ii vs c))
  ]
pp ii vs (Lam t c) =
  text "\\"
    <> text (vs !! ii)
    <> text ":"
    <> printType t
    <> text ". "
    <> pp (ii + 1) vs c

--ej1
pp ii vs (Let t1 t2) = sep 
  [ text "let"
  , text (vs !! ii)
  , text "="
  , parensIf (isNoTerminal t1) (pp ii vs t1)
  , text "in"
  , pp (ii + 1) vs t2
  ]
--ej2
pp _  _  Unit = text "unit"
--ej4
pp ii vs (Pair t1 t2) = parens $ sep [ pp ii vs t1
                                     , text ","
                                     , pp ii vs t2
                                     ]
pp ii vs (Fst t) = text "fst " <> parensIf (isNoTerminal t) (pp ii vs t)
pp ii vs (Snd t) = text "snd " <> parensIf (isNoTerminal t) (pp ii vs t)
--ej5
pp ii vs Zero    = text "0"
pp ii vs (Suc t) = text "suc " <> parensIf (isNoTerminal t) (pp ii vs t)
pp ii vs (Rec t1 t2 t3) = sep [ text "R"
                              , parensIf (isNoTerminal t1) (pp ii vs t1)
                              , parensIf (isNoTerminal t2) (pp ii vs t2)
                              , parensIf (isNoTerminal t3) (pp ii vs t3)
                              ]

isLam :: Term -> Bool
isLam (Lam _ _) = True
isLam _         = False

isNoTerminal :: Term -> Bool
isNoTerminal (Free _) = False
isNoTerminal (Bound _) = False
isNoTerminal (Pair _ _) = False
isNoTerminal t | t == Unit || t == Zero = False
isNoTerminal t = True

-- pretty-printer de tipos
printType :: Type -> Doc
printType EmptyT = text "E"
printType (FunT t1 t2) =
  sep [parensIf (isFun t1) (printType t1), text "->", printType t2]
--ej2
printType UnitT = text "Unit"
--ej4
printType (PairT t1 t2) = parens $ sep [ printType t1
                                       , text ","
                                       , printType t2
                                       ]
--ej5
printType NatT = text "Nat"

isFun :: Type -> Bool
isFun (FunT _ _) = True
isFun _          = False

-- Dado un término `t`, devuelve una lista con los nombres de las variables globales
-- que aparecen en `t`.
-- Utilizada en printTerm para quitar las variables globales
-- de `t` de la lista de posibles nombres para variables.
fv :: Term -> [String]
fv (Bound _         ) = []
fv (Free  (Global n)) = [n]
fv (t   :@: u       ) = fv t ++ fv u
fv (Lam _   u       ) = fv u
--ej1
fv (Let t1 t2       ) = fv t1 ++ fv t2
--ej2
fv Unit               = []
--ej4
fv (Pair t1 t2      ) = fv t1 ++ fv t2
fv (Fst t           ) = fv t
fv (Snd t           ) = fv t
--ej5
fv Zero               = []
fv (Suc t           ) = fv t
fv (Rec t1 t2 t3    ) = fv t1 ++ fv t2 ++ fv t3


---
printTerm :: Term -> Doc
printTerm t = pp 0 (filter (\v -> not $ elem v (fv t)) vars) t

