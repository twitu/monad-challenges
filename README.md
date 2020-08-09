# monad-challenges

The monad challenges is a programming project that tries to develop an intuitive understand of monads by implementing them and using them. It firsts asks you to implement milestones in a naive/brute force manner and slowly abstracts away general parts. This transformation of the code from simple to highly idiomatic is very though provoking and creates and intuition for what monads are actually doing.

If you are attempting this challenge try not to refer to solutions as the whole purpose of the challenge is to think through them yourself. One aspect that is not covered in the original challenges is the writing tests. This is done mostly for simplicitly. However I feel a test driven approach to this challenge makes debugging and implementing much easier you can refer to the `test` section below.

## Insights

1. Set 3 Part 2 - The application of `allPairs` to `allCards` is the an example of how high level functions (in this case data creating function) reduce code duplication and express logic elegantly. It shows how common patterns of iteration and composing can be abstracted away, since even the data constructor is a function as a parameter.

2. Set 3 Part 3 - `combStep :: [a -> b] -> [a] -> [b]` Is an excellent example that in functional programming things are best represented recursively and in the smallest unit of logic possible. This comb step was derived from the thought that a function can take elements are from an iterator and keep applying it to itself and generating partial functions. It will continue to do this till it is completely applied/evaluated to represent the value of its computation. It points to the intuition smallest logical unit a function with just one parameter. This function can be composed to make more complex logic.

3. Set 4 Part 5 - Using the bind function to implement `generalPair` is an eye opener. It reveals how sequencing works with monading functions. It's like three functions are puzzle pieces that fir together.
    - `Gen` is the starting piece. It starts of by wrapping a piece of computation into a newtype.
    - `bind` is the sequencer it threads the seeds through the functions that are calling it.
    - `return` collects the results of a computation. It has catches the seed that was being threaded and brings it together with teh result value. The best part is that `return` returns a value of type `Gen` so it can be further composed with other `Gen` functions.

## Tests

Writing tests will make the challenges much easier to play around with. Take a look at `Spec.hs`, I've imported `tasty-hunit`, `tasty` modules in the cabal file to drive the test suite. The tests are grouped in the three logical sets. There is no module 4 and 5 since they are only about making the first three modules more idiomatic. The test cases help ensure that even after changes the functions behave as before.
