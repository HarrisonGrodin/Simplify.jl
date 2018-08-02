# Rewrite.jl

[![Travis Build Status](https://travis-ci.org/HarrisonGrodin/Rewrite.jl.svg?branch=master)](https://travis-ci.org/HarrisonGrodin/SymReduce.jl)
[![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/a59v394qf05c7uec/branch/master?svg=true)](https://ci.appveyor.com/project/HarrisonGrodin/rewrite-jl/branch/master)
[![Coverage Status](https://coveralls.io/repos/github/HarrisonGrodin/Rewrite.jl/badge.svg?branch=master)](https://coveralls.io/github/HarrisonGrodin/SymReduce.jl?branch=master)

**Rewrite.jl** implements methods for rewriting symbolic terms in the Julia language based on custom, domain-specific axioms and properties.

## Motivation
Term rewriting is essential to a wide variety of fields, including elementary, boolean, and abstract algebras. Because existing symbolic simplification tools in Julia typically operate within fixed domains, it is difficult to obtain a level of control which allows for flexible inclusion and exclusion of specific mathematical properties. Rewrite.jl is a term rewriting library for Julia that simplifies algebraic expressions based on custom, domain-specific axioms.

## Approach
Rewrite.jl uses matching, normalization, and completion, which will be elaborated in the next sections.

### Matching
An expression can be matched against a user-defined pattern. The expression matches the pattern if:
1.  Name of expression matches name of pattern.
2.  All children of expression match corresponding children of pattern.
##### Examples
```
match(a - b, sin(x + y) ^ 2 - 10) => match
  a => sin(x + y) ^ 2
  b => 10
```
```
match(a * b, c() * log(c)) => match
  a => c()
  b => log(c)
```
```
match(g(z), h(z)) => no match
  g(z) != h(z)
```
```
match(f(a, a), f(cos(y), 15)) => no match
  cos(y) != 15
```

#### Predicates
It is often useful to include specific range of values for variables, such as even numbers, nonzero numbers, or integers in the set `{1,3,7}`, to more precisely represent some matching rules. These predicates can be attached to both patterns and expressions.
##### Examples
```
match(√(a) where a ≥ 0, √(abs(-y ^ 2 - 108))) => match
  a => abs(-y ^ 2 - 108)
```
```
match(b where b ∈ Odd, x - 1 where x ∈ [2, 4]) => match
  b => x - 1
```

#### Properties

### Normalization


### Completion
