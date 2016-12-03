# Haskell FSA

Made an implementation of a Deterministic finite state automaton in Haskell to learn Haskell syntax.

fsa is a function that takes in
- A string x to process
- A list of delta transitions d, where a transition functions is of the form (int initial state, char transition symbol, int final state)
- A list of final states int f
- A starting state int s
Then returns if the string x is in the language accepted by the machine