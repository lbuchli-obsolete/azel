module Language where

import Data.Word

newtype L1Prog = Prog [L1Item]                               -- program
data L1Item = FuncDecl L1FuncDecl | TypeDecl L1TypeDecl      -- program item

type L1FuncDecl = (L1FuncSignature, [L1FuncVariant])         -- function declaration
data L1FuncSignature                                         -- function signature
  = SAp L1FuncSignature L1FuncSignature
  | SType Type
  | SSymbol Symbol
type L1FuncVariant = ([Either L1TypeVariant Symbol], L1Expr) -- function variant

type L1TypeDecl = (L1TypeSignature, [L1TypeVariant])         -- type declaration
type L1TypeVariant = [Either Type Symbol]                    -- type variant
type L1TypeSignature = [Either Type Symbol]

data L1Expr                                                  -- expression
  = EVar Symbol          -- variable
  | EByte Word8          -- byte
  | EAp L1Expr L1Expr    -- function application
  | ELet                 -- let expression
    [(Symbol, L1Expr)]   --   variable definitions
    L1Expr               --   body
  | ECase                -- case expression
    L1Expr               -- expression to scrutinize
    [L1FuncVariant]      --   variants

type Symbol = String                                       -- symbol
data Type = Concrete String | Polymorphic Char             -- type; concrete or polymorphic

isAtomicExpr :: L1Expr -> Bool
isAtomicExpr (EVar _)  = True
isAtomicExpr (EByte _) = True
isAtomicExpr _         = False

  
------------------------------------------------------
--                  Pretty-Printing                 --
------------------------------------------------------

instance Show L1Expr where
  show (EVar v)          = v
  show (EByte b)         = show b
  show (EAp a b)         = "(" ++ show a ++ " " ++ show b ++ ")"
  show (ELet defs body)  = "let " ++ showDefs defs ++ " in " ++ show body
  show (ECase expr vars) = "switch (" ++ show expr ++ ") \n" ++ showFuncVars 2 vars

instance Show L1FuncSignature where
  show (SAp a b)               = "(" ++ show a ++ " " ++ show b ++ ")"
  show (SType (Concrete s))    = s
  show (SType (Polymorphic c)) = [c]
  show (SSymbol s)             = "'" ++ s ++ "'"

instance Show L1Item where
  show (FuncDecl (sig, vars)) = ":: " ++ show sig ++ showFuncVars 1 vars
  show (TypeDecl (sig, vars)) = ":> " ++ showTypeSig sig ++ showTypeVars vars

instance Show L1Prog where
  show (Prog items) = foldl (\p n -> p ++ "\n\n" ++ show n) "[prog] where" items

instance Show Type where
  show (Concrete s) = s
  show (Polymorphic c) = [c]


showDefs :: [(Symbol, L1Expr)] -> String
showDefs = foldl (\p n -> p ++ ", " ++ fst n ++ " = " ++ show (snd n)) ""

showFuncVars :: Int -> [([Either L1TypeVariant Symbol], L1Expr)] -> String
showFuncVars indent = foldl (\p n -> p ++ "\n" ++ tabs ++ show (fst n) ++ " = " ++ show (snd n)) tabs
  where tabs = concat $ replicate indent "\t"

showTypeSig :: L1TypeSignature -> String
showTypeSig = undefined

showTypeVars :: [[Either Type Symbol]] -> String
showTypeVars = foldl (\p n -> p ++ " | " ++ variant n) ""
  where
    variant = foldl (\p n -> p ++ " " ++ item n) ""
    item (Left t) = show t
    item (Right s) = "'" ++ s ++ "'"

  
----------------------------------------------------------------
--                           Prelude                          --
----------------------------------------------------------------

prelude :: L1Prog
prelude = Prog [
  FuncDecl (SAp (SType (Polymorphic 'x')) (SType (Polymorphic 'x')), [([Right "x"], EVar "x")])
               ]
