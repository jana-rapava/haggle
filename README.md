# Haggle

Haggle is a search language for general state-space search problems. It aims to make combining different search algorithms and trying different heuristics simple.

## Compilation and loading

Source code of Haggle consists of core modules - Haggle, Path and Expandable - and search algorithm modules. Supported search algorithms are:

* best-first search (with heuristics)

* breadth-first search (with any, even infinite, depth)

In order to use Haggle, it's necessary to have GHC 7 installed. Later versions will probably not work.

The easiest way to use Haggle is to keep the source code files where you downloaded them, and give the particular directory as -i option to the compiler when you are compiling/running your Haggle files.

## Adding new search algorithms

Implement your search algorithm in Haskell in a new Haskell module. Top-level function which does the actual search needs to have a return type State (Backlog a) [[a]] for a member of the Expandable typeclass.

Then just import this module from your Haggle file and use the function.

## Trying out new problems

In order for the problem to be solved by Haggle, you have to make the problem representation a member of typeclass Expandable.

Now you can use available search algorithms to find solutions for your problem.

## Haggle file format

Haggle files are normal Haskell files. You are free to integrate Haskell snippets into your code wherever compiler allows it.

Scripts in Haggle look like the following:

```
import Haggle
<import used search algorithms>
<import other modules>`

main = < result selector > $ do
                                <search algorithm 1> <params>
                                ...
                                <search algorithms n> <params>
```

To use best-first search, use:

`befs < heuristic function >`

To use breadth/first search, use:

`bfs < limit; Inf for infinity >`

To run your Haggle files, use:

`runghc -i<path to Haggle source> hagglefile.hs`

## Examples

A collection of example files is in directory examples. These examples are all concerned with finding solutions for Lloyd 15 problem in various ways.

All examples may be built and run by shell script runExamples.sh.

## Tests

Unit tests are in the directory tests. They use HUnit framework.

Module TestFixtures contains some helper functions for heuristics and data for testing.
Module TestLloyd15 tests representation of Lloyd15 problem.
Module TestBeFS2x2 searches for solutions to 2x2 version of Lloyd15 using best-first search.
Module TestBeFS4x4 searches for solutions to 4x4 version of Lloyd15 using best-first search.
Module TestBFS2x2 searches for solutions to 2x2 version of Lloyd15 using breadth-first search.
Module TestBFS4x4 searches for solutions to 4x4 version of Lloyd15 using breadth-first search.

All tests may be run by shell script runTests.sh.
