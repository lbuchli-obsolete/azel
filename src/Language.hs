module Language where

import Data.Word

newtype AzProg = Prog [AzItem]                               -- program
data AzItem = FuncDecl AzFuncDecl | TypeDecl AzTypeDecl      -- program item

type AzFuncDecl = (AzFuncSignature, [AzFuncVariant])         -- function declaration
data AzFuncSignature                                         -- function signature
  = SAp AzFuncSignature AzFuncSignature
  | SType Type
  | SSymbol Symbol
type AzFuncVariant = ([Either AzTypeVariant Symbol], AzExpr) -- function variant

type AzTypeDecl = (AzTypeSignature, [AzTypeVariant])         -- type declaration
type AzTypeVariant = [Either Type Symbol]                    -- type variant
type AzTypeSignature = [Either Type Symbol]

data AzExpr                                                  -- expression
  = EVar Symbol          -- variable
  | EByte Word8          -- byte
  | EAp AzExpr AzExpr    -- function application
  | ELet                 -- let expression
    [(Symbol, AzExpr)]   --   variable definitions
    AzExpr               --   body
  | ECase                -- case expression
    AzExpr               -- expression to scrutinize
    [AzFuncVariant]      --   variants

type Symbol = String                                       -- symbol
data Type = Concrete String | Polymorphic String           -- type; concrete or polymorphic

isAtomicExpr :: AzExpr -> Bool
isAtomicExpr (EVar _)  = True
isAtomicExpr (EByte _) = True
isAtomicExpr _         = False

  
------------------------------------------------------
--                  Pretty-Printing                 --
------------------------------------------------------

instance Show AzExpr where
  show (EVar v)          = v
  show (EByte b)         = show b
  show (EAp a b)         = "(" ++ show a ++ " " ++ show b ++ ")"
  show (ELet defs body)  = "let " ++ showDefs defs ++ " in " ++ show body
  show (ECase expr vars) = "switch (" ++ show expr ++ ") \n" ++ showFuncVars 2 vars

instance Show AzFuncSignature where
  show (SAp a b)               = "(" ++ show a ++ " " ++ show b ++ ")"
  show (SType (Concrete s))    = "<" ++ s ++ ">"
  show (SType (Polymorphic s)) = s
  show (SSymbol s)             = "'" ++ s ++ "'"

instance Show AzItem where
  show (FuncDecl (sig, vars)) = ":: " ++ show sig ++ showFuncVars 1 vars
  show (TypeDecl (sig, vars)) = ":> " ++ showTypeSig sig ++ showTypeVars vars

instance Show AzProg where
  show (Prog items) = foldl (\p n -> p ++ "\n\n" ++ show n) "[prog] where" items

instance Show Type where
  show (Concrete s) = "<" ++ s ++ ">"
  show (Polymorphic s) = s


showDefs :: [(Symbol, AzExpr)] -> String
showDefs = foldl (\p n -> p ++ ", " ++ fst n ++ " = " ++ show (snd n)) ""

showFuncVars :: Int -> [([Either AzTypeVariant Symbol], AzExpr)] -> String
showFuncVars indent = foldl (\p n -> p ++ "\n" ++ tabs ++ show (fst n) ++ " = " ++ show (snd n)) tabs
  where tabs = concat $ replicate indent "\t"

showTypeSig :: AzTypeSignature -> String
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

prelude :: AzProg
prelude = Prog [
  FuncDecl (SAp (SType (Polymorphic "x")) (SType (Polymorphic "x")), [([Right "x"], EVar "x")])
               ]
