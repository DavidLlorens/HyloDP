# HyloDP
A package for  solving dynamic programming problems in Haskell

## Introduction
This package contains the library HyloDP and six solved DP problems: Edit Distance, Fibonacci, Knapsack, Longest Common Subsequence, Random Walk and Text Segmentation.

This package contains the code of the research article:
Software: Practice and Experience (ISSN:1097-024X)
>"Easily solving dynamic programming problems in Haskell by memoization of hylomorphisms" by D.Llorens and J.M. Vilar. Software: Practice and Experience (ISSN:1097-024X). 2020; 50: 2193–2211. [https://doi.org/10.1002/spe.2887](https://doi.org/10.1002/spe.2887).

A preliminary version of the paper can be downloaded from [here](https://repositori.uji.es/xmlui/bitstream/handle/10234/191226/71752.pdf).

The core ideas are:

* Using hylomorphisms as generic control flow mechanism.

* Adding memoization to avoid recomputation of intermediate results.

* Modelling the composition of solutions by means of semirings.

* Using dispatch by result type as a simple way to decide the
kind of answer desired (e.g. the best
solution, the score of that solution, or the total number of
solutions.)

A problem is described by an instance of `DPProblem` and solved by
using `dpSolve`. See the documentation of `HyloDP.Base` for an
example.
## Contents
* The library is contained in the [src](https://github.com/DavidLlorens/HyloDP/tree/master/src) directory.
* The [test](https://github.com/DavidLlorens/HyloDP/tree/master/test) directory contains several HSpec tests.
* There are six examples in the [examples](https://github.com/DavidLlorens/HyloDP/tree/master/examples) directory.
* The  `run-examples.sh` script runs all the examples.
* This library is implemented as a package [on Hackage](https://hackage.haskell.org/package/HyloDP).