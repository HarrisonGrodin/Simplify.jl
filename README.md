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
An expression can be matched against a user-defined pattern.

#### Predicates


#### Properties

### Normalization


### Completion
