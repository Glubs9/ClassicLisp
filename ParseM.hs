{- 
--this was just me throwing my ideas at the wall. delete it later

ideas:
push down parsing cause that's real cool 
	(oh shit I should finish that mit course lol)
so this is kinda hard
most things should be alright but the real trouble comes in differentiating buetween the three different lists, so i say fuck it we just make em all the same type.
	- i don't think that would work for condition statement
	- although i should just be able to check i guess
		- to do that we write function which is find arrow on same scope
		- and if found mark it as a condition expression i guess
this won't exactly have the context rules specified in the data type because of my failings as a coder
	- because i wat there only to be one data type for the easier implementation and thusly
	- i cannot have recursive type on the type of thing
		- eg: data Tmp = Normal | Recursed Normal
			- will throw error but 
		- eg: data Tmp = Normal | Recursed Tmp
			- will not throw error but will basically sidestep haskell type checking
			- which is not fantastic
i will also have to check for = but that is also another problem
	- like cause i can't have just search for equals cause like it's only gotta serach for
	- token directly after closed bracket (ah there we go good job)

dotted pairs: (X . Y) --> X.Y
Lists: (A B C) --> [A;B;C]
Application: (F X) --> F[X]
Boolean: (OR P (AND Q R)) ---> P \/ Q /\ R
COND: (COND (P A) (Q B) (T C)) --> P->A; Q->B; C
DEFUN: (DEFUN A (B) (...)) --> A[B] = ...
-}

-- this parser will error on (()) it will interpret this as a list containing only ( and a trailing )
-- it also doesn't support higher order functions

module ParseM where

import LexM

--app String Expr removes possibility for higher order functions (this is just for ease of
		--implementation and should probably be updated later (although it shouldn't be too
			--hard it's like whatever i'm doing it this way (i mean it might still work
				--in some cases like passing it as a variable it should work but not
				--if it's not set to a variable like it's the output of a function
				--or something)))
data Expr = Definition String Expr Expr | ConsList Expr | Cons Expr Expr | CondPair Expr Expr | Pair Expr Expr | App String Expr | And Expr Expr | Or Expr Expr | CondList Expr | Var String | Empty | EmptyList | Open | Closed | CondOpen | CondClosed | Dot | Colon | AndSym | OrSym | Arrow | Equals | NewLine | OpenPrecedence | ClosedPrecedence deriving (Show, Eq)
-- below are some explanations for what the expressions are and need
	--also note: that this should be encoded in a context free grammar but that doesn't work
	--with haskell type checking so I did this to basically side-step the type checking
-- defintion name arglist function_value
-- we will define pairs as cons and lists as cons that end with empty
-- app might be string Expr but I think this will allow for functions to be returned maybe? i
-- might never use them though? (maybe for like pointfrree shit but idk
-- cond takes a list of pairs and converts it to a list of pairs but it's a function 
-- all the constants are for temporary token stuff
	-- except for empty and emptylist with emtpy used for the end of a list and emptylist used
	-- for an emptylist
	
-- the below function is a bit of a misuse of pattern matching, i would have written this as a dictionary if i had written
-- it in any other language but i haven't bothered looking it up in haskell
	-- or if there was a way to subclass other data types
convert_tok :: Token -> Expr
convert_tok TOKOpen = Open
convert_tok TOKClosed = Closed
convert_tok TOKDot = Dot
convert_tok TOKColon = Colon
convert_tok (TOKVar s) = (Var s)
convert_tok TOKAnd = AndSym
convert_tok TOKOr = OrSym
convert_tok TOKArrow = Arrow
convert_tok TOKDefine = Equals
convert_tok TOKCondOpen = CondOpen
convert_tok TOKCondClosed = CondClosed
convert_tok TOKNewLine = NewLine
convert_tok TOKOpenPrecedence = OpenPrecedence
convert_tok TOKClosedPrecedence = ClosedPrecedence

add_empty_to_list :: Expr -> Expr
add_empty_to_list (Cons e1 e2) = (Cons e1 (add_empty_to_list e2))
add_empty_to_list e = (Cons e Empty)

-- handle precedence cause fuck i can't be bothered to do precedence (or like i haven't wrote this parser like that so fuckkkk me)
-- this parser does do a little bit of trivial compilation to make the rest of the program easier
parse :: [Token] -> [Expr] -> [Expr] --change back to expr after debugging
-- parse tokens stack = ast
-- wait does this eval properly? maybe condclosed is defined like five times???
parse tokens stack
	| (Closed:e1:Colon:e2:next) <- stack			= next_func $ (Closed:(Cons e2 e1):next)
	| (CondClosed:e1:Colon:e2:next) <- stack		= next_func $ (CondClosed:(Cons e2 e1):next)
	| (OpenPrecedence:Dot:e:next) <- stack			= skip_eval
	| (e1:Dot:e2:next) <- stack				= next_func $ ((Pair e2 e1):next)
	| (CondClosed:e1:Arrow:OpenPrecedence:next) <- stack	= skip_eval
	| (CondClosed:e1:Arrow:e2:next) <- stack		= next_func $ (CondClosed:(CondPair e2 e1):next)
	| (Colon:e1:Arrow:e2:next) <- stack			= next_func $ (Colon:(CondPair e2 e1):next)
	| (Closed:e:Open:next) <- stack				= next_func $ ((ConsList (add_empty_to_list e)):next)
	| (Closed:Open:next) <- stack				= next_func $ (EmptyList:next)
	| (CondClosed:e:CondOpen:next) <- stack                 = next_func $ ((CondList (add_empty_to_list e)):next)
	| (ClosedPrecedence:e:OpenPrecedence:next) <- stack     = next_func $ (e:next)
	| (NewLine:e:equals:(App str param_list):next) <- stack = next_func $ ((Definition str param_list e):next)
	| (arg_list@(ConsList e):(Var str):next) <- stack	= next_func $ ((App str arg_list):next) -- this line removes higher order functions and i think they ight be a thing in the original lisp idk have to check later
	| (OpenPrecedence:AndSym:e:next) <- stack		= skip_eval
	| (e1:AndSym:e2:next) <- stack				= next_func $ ((And e2 e1):next)
	| (OpenPrecedence:OrSym:e:next) <- stack		= skip_eval
	| (e1:OrSym:e2:next) <- stack				= next_func $ ((Or e2 e1):next)
	| tokens == []						= stack -- will cause error if code (or parser) is written poorly (in debugging i changed it to stack while it was previously head stack)
	| otherwise						= skip_eval
	where next_func = parse tokens
	      skip_eval = parse (tail tokens) $ (convert_tok $ head tokens):stack

parseM = (flip parse) []

test_func = parseM . lexM --for debugging, delete later
