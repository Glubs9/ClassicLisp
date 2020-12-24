# ClassicLisp
a compiler that compiles the original lisp syntax to scheme. 

# Motivation and Explanation
I read the original lisp paper for fun and thought that it couldn't be *tooo* hard to implement (this was quite foolish of me).                   
this is where i read the paper http://www-formal.stanford.edu/jmc/recursive.html                     
After some brief reserach I discovered that the m-expression syntax described in the original paper was more of an informal slang than any properly defined syntax.              
To get around this I decided that I should just stick to the most well defined syntax I found here: https://wiki.c2.com/?EmExpressions.                
I had also just read the mit course on push down automata so I wanted to implement it using that. This was done with a complete lack of thought regarding operator precedence.                
I originally wanted to implement a full standard library and an interpreter for the compiled s expressions but I ran out of motivation so I just changed the compiler to output scheme expressions.                     
The compilers ended up buggy and only half working but I had fun making it.                 
I will probably never end up making this perfectly functional but hey, what are you gonna do.              

i'm not going to fix this any more unless i get another stroke of motivation.          

# important notes
If you really do want to run this remember that the operator precedence is fucked so if it's not compiling properly try putting lots of brackets () around everything. It also might just be that you've found a bug which is not unlikely. 

# installation
download all files and compile Main.hs using any ol' haskell compiler (only tested with ghc).              

# running
to run the program with file inputs use the command: cat \[file_name] | ./Main (or equivalent windows statement)                      
you can also enter the program by typing ./Main and entering the code followed by pressing CTRL+d to run the code.                 

# syntax distinctions
due to my terrible compiler and lack of effort I changed the syntax from the original specification to make it easier to compile.                     
the and symbol for booleans has been replaced by &. or has been replaced with |. the arrow symbol -> has been replaced by >. the list separator ; eg: \[a;b;c] is now , eg: \[a,b,c].                     
the condition list identifier \[ and ] eg: \[T -> F; F -> T] is now { and } eg: {T > F, F > T}.                
function definitions have to end with a semicolon. eg: atom[x] = not[list?[x]];               

# basic example
in the original paper ff[x] is defined as getting the first item in a list regardless of indentation.                 
the original paper defines this as ff[x] = [atom[x] -> x;T -> ff[car[x]]]              
and in my compiler it can be written as ff[x] = {not[list?[x]] > x, else > ff[car[x]]};                      
