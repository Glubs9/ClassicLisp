import LexM
import ParseM
import CompileM
import Debug.Trace

-- write func here

main_func = compileM . parseM . lexM

main = do
	contents <- getContents
	putStrLn $ main_func contents
