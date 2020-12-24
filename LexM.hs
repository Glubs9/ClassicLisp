module LexM where

-- idk about if tokcondopen is strictly necersarry
data Token = TOKCondOpen | TOKCondClosed | TOKOpen | TOKClosed | TOKDot | TOKColon | TOKVar String | TOKAnd | TOKOr | TOKArrow | TOKDefine | TOKNewLine | TOKOpenPrecedence | TOKClosedPrecedence deriving (Show, Eq)

-- all the token conversions are one character long to make lexing easier
convert :: String -> Maybe Token
convert "["  = Just TOKOpen
convert "]"  = Just TOKClosed
-- convert "("  = Just 
convert "."  = Just TOKDot
convert ","  = Just TOKColon
convert "&" = Just TOKAnd
convert "|" = Just TOKOr
convert ">" = Just TOKArrow
convert "="  = Just TOKDefine
convert "{" = Just TOKCondOpen
convert "}" = Just TOKCondClosed
convert ";" = Just TOKNewLine
convert "(" = Just TOKOpenPrecedence
convert ")" = Just TOKClosedPrecedence
convert _ = Nothing

convert_known :: String -> Token
convert_known str
	| convert_out == Nothing    = (TOKVar str)
	| (Just tok) <- convert_out = tok
	| otherwise		    = error "(in convert_known) unreachable code"
	where convert_out = convert str

-- if you're cleaning up the function see if you can use pattern matching to extract first_char and tail_str
tokenize :: [Char] -> [Char] -> [Token]
tokenize string_input prev_input
	| string_input == []                      = [(convert_known prev_input)]
	| first_char == ' ' || first_char == '\n' = (convert_known prev_input):(next_func "") --space reached, add prev_input to output (not necersarry due to filter_empty_vars)
	| (Just tok) <- convert_out               = (convert_known prev_input):tok:(next_func "") --token found, add prev_input and first char to output
	| convert_out == Nothing		  = next_func (prev_input ++ [first_char]) --nothing found, continue recurse (not efficient but easier to reason about, change later)
	| otherwise				  = error "(in tokenize) unreachable code"
	where first_char  = head string_input
	      convert_out = convert [first_char]
	      next_func   = (tokenize . tail) string_input

not_empty_var :: Token -> Bool
not_empty_var (TOKVar "") = False
not_empty_var (TOKVar "\n") = False
not_empty_var e = True

filter_empty_vars :: [Token] -> [Token]
filter_empty_vars = filter not_empty_var

lexM :: String -> [Token]
lexM = filter_empty_vars . (flip tokenize $ "") --could be defined point free but something is going wrong here?
