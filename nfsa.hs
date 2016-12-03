

main = putStrln(nfsa "abc" 2 3 4)


{-
d = [
    (1, [('1', 2), ('2', 3), ('3', 1)]),
    (2, [('1', 3), ('2', 2), ('3', 1)]),
    ]
-}

nfsa x d f s = map start ((map string nfsa) d f) where


    -- Check if there is a state with transitions corresponding to s
    let deltaTransitions = if not null state then state !! 0 else []
    -- Search through the symbols and get the valid ones given input
    in let delta = filter (\y -> fst y == '' || fst y == symbol) deltaTransitions
    -- Fetch the end states
    in start = map snd delta
    string = map fst delta
    -- Sort through the state and transitions to find the one corresponding to s
    where state = filter (\y -> fst y == s) s 
    where symbol = if null x then '' else x !! 0

