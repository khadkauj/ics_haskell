module BoolExpr (Variable, BoolExpr(..), evaluate) where
import Data.List
import System.IO

type Variable = Char
data BoolExpr = T
 |F
 |Var Variable
 |Not BoolExpr
 |And BoolExpr BoolExpr
 |Or BoolExpr BoolExpr
 deriving(Show)


-- evaluates an expression
evaluate :: BoolExpr -> [Variable] -> Bool
evaluate T _            = True
evaluate F _            = False
evaluate (Var v) vs     = v `elem` vs
evaluate (Not e) vs     = not (evaluate e vs)
evaluate (And e1 e2) vs = evaluate e1 vs && evaluate e2 vs
evaluate (Or  e1 e2) vs = evaluate e1 vs || evaluate e2 vs


variables :: BoolExpr -> [Variable]
variables T = ""
variables F = ""
variables (Or T F) = ""
variables (Var v) = [v]
variables (Or F T) = ""
variables (And T F) = ""
variables (And F T) = ""
variables (Not e) = variables (e)
variables (And e1 e2) = sort $ union (variables e1) (variables e2) 
variables (Or e1 e2) = sort $ union (variables e1)  (variables e2)

{-variables function takes boolexpression as input and gives string as output;
for only T or F or theirr  combination it returns empty string;
when a valiad boolean expression is given it again calls the function and executes the
command in line 29; same command is used for Not bool expression-}

subsets :: [Variable] -> [[Variable]]
subsets [] = [[]]
subsets(x:xs) = subsets xs ++ map (x:) (subsets xs)
{-this takes string and returns set of string which are it's subsets;
we recurse through tail of this string and concatenate it mapping of
head, hence making combinations of string-}



truthtable :: BoolExpr -> [([Variable], Bool)]
truthtable x = [(substr, evaluate x substr) | substr <- subsets (variables x)] 
{-we use above two functions to acheive this; we proviede list comperehension: using subsets to
cretae subsets and varibales to check boolean value-}



main = print()