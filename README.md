# Playground

## CIS 194
Solutions to the UPenn's [CIS 194: Introduction to Haskell (Spring 2013)](https://www.seas.upenn.edu/%7Ecis194/spring13/) class. **Note that some of the code was provided as part of the assignment.**

## Data61
Some solutions to the Data61's (NICTA) [Functional Programming Course](https://github.com/data61/fp-course).

## H-99
Some solutions to the [H-99: Ninety-Nine Haskell Problems](https://wiki.haskell.org/H-99:_Ninety-Nine_Haskell_Problems).

## Quadratic sieve
Basic implementation of the quadratic sieve algorithm in Haskell using the `arithmoi` package. Usage:
```
factor 9345673029867
```
More involved implementation of the self-initializing quadratic sieve algorithm is available in `siqs.hs`. Function `factorise` finds one factor of a hard-coded prime. The algorithm is missing a randomness element in the `computeAParamenter` function to make it useful universally.
