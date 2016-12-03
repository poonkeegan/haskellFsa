 
main = do 
    let loop = do 
        putStrLn "What string would you to check in the language"
        input <- getLine
        putStrLn(if fsa input [(0, '0', 0), (0, '1', 1), (1, '0', 1), (1, '1', 0)] [1] 0 then "TRUE" else "FALSE")
        loop
    loop



fst3 (x,_,_) = x
snd3 (_,y,_) = y
thd3 (_,_,z) = z
{-
x: input string
d: delta transitions [int state, char symbol, int next state]
s: starting state
f: set (list) of final states
Returns true if x is in the language denoted by the FSA
-}
fsa x d f s =

    -- Base case, finished
    if null x
        then elem s f
        
        else

            -- Check for the transitions out of current state
            let filterCond = \y -> fst3 y == s && snd3 y == x !! 0

            -- Generate arguments to pass to FSA
            in let newFsa = fsa (tail x) d f
            in let getState = \y -> thd3 y
            in let moveStates = map getState (filter filterCond d)
            in let leadToEnd = map newFsa moveStates
            in elem True leadToEnd

