# Clojure Solutions

I store everything under `src/advent2017/core.clj`.

To play: `lein repl` in the root of this repository.

### Conventions

Entry points to solutions are all `problemN_pM` where N is in the range `1-25` inclusive, and `M` is in the range `1-2` inclusive.

Data is all under namespace `advent2017.data`, and has the names `dataN`, where N is the same as the associate problem.

All problem functions (`problemN_pM`) take a single input containing the input, sometimes as string, sometimes as a vector. Whatever makes sense for the function.

### Fast usage

To run a problem, say problem 5 part 1, with my data, just:

```
$ lein repl
> (use 'advent2017.core)
> (tryprob 5 1)
```

To try it with your data, just do as follows:

```
$ lein repl
> (use 'advent2017.core)
> (problem5_p1 [0 3 0 1 -3])
```

or substitute a string if it's that sort of problem.

### How to comment on code

Open an issue! Or you could comment inline, but nobody ever sees those again if
something changes.
