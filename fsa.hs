 
main = do 
    let loop = do 
        putStrLn "What string would you to check in the language"
        input <- getLine
        putStrLn(if fsa input [
            (1, '1', 4),
            (1, '2', 7), 
            (1, '3', 3), 
            (2, '1', 3),
            (2, '2', 9), 
            (2, '3', 4), 
            (3, '1', 2),
            (3, '2', 8), 
            (3, '3', 5), 
            (4, '1', 5),
            (4, '2', 6), 
            (4, '3', 2), 
            (5, '1', 4),
            (5, '2', 7), 
            (5, '3', 3), 
            (6, '1', 7),
            (6, '2', 4), 
            (6, '3', 9), 
            (7, '1', 6),
            (7, '2', 5), 
            (7, '3', 8),
            (8, '1', 9),
            (8, '2', 3), 
            (8, '3', 7), 
            (9, '1', 8),
            (9, '2', 2), 
            (9, '3', 6)] 
            [2,3,4,6,7,8,9] 1 then "TRUE" else "FALSE")
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
            in  let newFsa = fsa (tail x) d f
                    getState = \y -> thd3 y
            in let moveStates = map getState (filter filterCond d)
            in let leadToEnd = map newFsa moveStates
            in elem True leadToEnd

