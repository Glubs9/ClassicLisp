{-
this follows that document I found pretty much exaclty (though check the keywords used in the original paper
dotted pairs = dotted pairs
lists = lists
application = take argument list and concatenate function name to start and put through list builder
boolean expressions = compile to applications and put through application do-er
actually with conditions we will recieve a list of p > e and then we will add COND to the frong and  put it through lists and then when it finds p > e we just output (p e)
for definitions just make application of whatever fuck it's easy
-}

{-
dotted pairs: (X . Y) --> X.Y
Lists: (A B C) --> [A;B;C]
Application: (F X) --> F[X]
Boolean: (OR P (AND Q R)) ---> P \/ Q /\ R
COND: (COND (P A) (Q B) (T C)) --> P->A; Q->B; C
DEFUN: (DEFUN A (B) (...)) --> A[B] = ...
-}

module CompileM where

import ParseM

-- compile_cond :: (CondList e) -> String
compile_cond :: Expr -> String
compile_cond Empty = ")"
compile_cond (Cons (CondPair e1 e2) next) = "(" ++ (compile_expr e1) ++ " " ++ (compile_expr e2) ++ ") " ++ (compile_cond next)

compile_expr :: Expr -> String
compile_expr (Var e) = e
compile_expr (Pair e1 e2) = "( " ++ (compile_expr e1) ++ " . " ++ (compile_expr e2) ++ " )";
compile_expr (ConsList e) = "`( " ++ (compile_expr e)
compile_expr (Cons e Empty) = (compile_expr e) ++ " )"
compile_expr (Cons e1 e2) = (compile_expr e1) ++ " " ++ (compile_expr e2)
compile_expr (App str (ConsList e)) = "( " ++ str ++ " " ++ (compile_expr e)
compile_expr (Definition str (ConsList args) val) = "(define " ++ str ++ " (lambda ( " ++ (compile_expr args) ++ " " ++ (compile_expr val) ++ " ))) "
compile_expr (And e1 e2) = "(and " ++ (compile_expr e1) ++ " " ++ (compile_expr e2) ++ ") "
compile_expr (Or e1 e2) = "(or " ++ (compile_expr e1) ++ " " ++ (compile_expr e2) ++ ") "
compile_expr (Pair e1 e2) = "( " ++ (compile_expr e1) ++ "." ++ (compile_expr e2)
compile_expr (CondList e) = "(cond " ++ (compile_cond e) 
compile_expr EmptyList = "`()"

compileM :: [Expr] -> String
--compileM foldr (compile_expr +)
compileM [] = ""
compileM (n:next) = compile_expr n ++ (compileM next)
