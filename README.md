# Rewrite.jl

[![Travis Build Status](https://travis-ci.org/HarrisonGrodin/Rewrite.jl.svg?branch=master)](https://travis-ci.org/HarrisonGrodin/Rewrite.jl)
[![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/a59v394qf05c7uec/branch/master?svg=true)](https://ci.appveyor.com/project/HarrisonGrodin/rewrite-jl/branch/master)
[![Coverage Status](https://coveralls.io/repos/github/HarrisonGrodin/Rewrite.jl/badge.svg?branch=master)](https://coveralls.io/github/HarrisonGrodin/Rewrite.jl?branch=master)

**Rewrite.jl** implements methods for rewriting symbolic terms in the Julia language based on custom, domain-specific axioms and properties.

## Examples
Term rewriting can be applied to a wide variety of fields, including elementary, boolean, and abstract algebras.

Normalization involves determining the unique normal form of an expression ("simplest" equivalent expression) through repeated application of rules. *Rewrite.jl* will use its [internal set of algebraic rules](./src/rules.jl) by default, which includes trigonometry, logarithms, differentiation (based on [DiffRules.jl](https://github.com/JuliaDiff/DiffRules.jl)), and more.
```julia
julia> normalize(@term(1 / (sin(-θ) / cos(-θ))))
@term(-(cot(θ)))

julia> normalize(@term(log(b, 1 / (b^abs(x^2)))))
@term(-(x ^ 2))

julia> normalize(@term(diff(sin(2x) - log(x+y), x)))
@term((2 * one(x) + zero(x) * x) * cos(2x) - (1 / (x + y)) * (one(x) + zero(x)))

julia> normalize(@term(!x & x | (y & (y | true))))
@term(y)

julia> normalize(@term(y^(6 - 3log(x, x^2))))
@term(one(y))
```

If only specific sets of predefined rules are desired, they may be specified as follows.
```julia
julia> normalize(@term(sin(α)cos(α) - cos(α)sin(α)), :TRIGONOMETRY)
@term(sin(α - α))

julia> normalize(@term(sin(α)cos(α) - cos(α)sin(α)), :BASIC, :TRIGONOMETRY)
@term(0)
```

In many cases, it is useful to specify entirely custom rules by passing a Term Rewriting System as the second argument to `normalize`. This may be done either by manually constructing a `TRS` object or by using the `RULES` strategy for `@term`.
```julia
julia> normalize(@term(f(x, f(y, y))), @term RULES [
           f(x, x) => 1
           f(x, 1) => x
       ])
@term(x)

julia> normalize(@term(f(g(f(1), h()))), TRS(
           @term(f(x)) => @term(x),
           @term(h())  => @term(3),
       ))
@term(g(1, 3))

julia> using Rewrite: EvalRule

julia> normalize(@term(f(g(f(1), h()))), TRS(
           @term(f(x)) => @term(x),
           @term(h())  => @term(3),
           EvalRule(:g, (a, b) -> 2a + b)
       ))
@term(5)
```

Variables may contain information about their domain, which may result in more specific normalizations.
```julia
julia> using SpecialSets

julia> x = Variable(:x)
       y = Variable(:y, GreaterThan(3))
       z = Variable(:z, Even ∩ LessThan(0))
@term(z)

julia> normalize(@term(abs($x)))
@term(abs(x))

julia> normalize(@term(abs($y)))
@term(y)

julia> normalize(@term(abs($z)))
@term(-z)
```

```julia
julia> x, y = Variable.([:x, :y], Ref(TypeSet(Int)));

julia> normalize(@term(diff(sin(2*$x) - log($x+$y), $x)))
@term(2 * cos(2x) - 1 / (x + y))
```


## Approach
*Rewrite.jl* uses normalization via matching, which will be elaborated in the next sections.

 - [Matching](#matching) is used to compare the expression being normalized to the left side of normalization rules.
 - [Normalization](#normalization) involves the transformation of an expression based on one or more applications of rules.

### Matching
An subject expression can be matched against a pattern expression to determine whether or not the subject is structurally similar to the pattern. If the matching process succeeds, it generates one or more substitutions which transform the pattern into the subject.

Semantic matching succeeds if and only if:
  1. The pattern is a variable.
  2. The pattern and subject are equivalent constants.
  3. The pattern and subject are functions with the same name and arity and every argument of the pattern matches the corresponding argument of the subject.

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

#### Properties
Many functions have implicit properties which affect the ways in which they should be matched. Orderless (commutative) functions are matched without respect to argument order, while flat (associative) functions are matched such that a variable in the pattern can match multiple arguments in the subject. Many functions have one or both of these properties. For example, `+` is by default orderless and flat, and `*` is by default flat. Properties are configurable and may be derived from the rewrite domain.

##### Examples
```
Orderless and flat:
match(f(a) + g(b) + c, x ^ 4 + g(tan(y)) + 3 + log(√(x)) + f(y)) => match
  a => y
  b => tan(y)
  c => x ^ 4 + 3 + log(√(x))
```
```
Flat only:
match(a * f(b), x * y^z * f(w)) => match
  a => x * y^z
  b => w
```

#### Constraints
It is often useful to specify a range of values which variables may take, such as even numbers, nonzero numbers, or integers in the set `{1, 3, 7}`, to more precisely represent some matching rules. These [images](https://en.wikipedia.org/wiki/Image_(mathematics)) can be attached to any term.

##### Examples
```
Let a ≥ 0.
match(√(a), √(abs(-y ^ 2 - 108))) => match
  a => abs(-y ^ 2 - 108)
```
```
Let b be odd, x ∈ {2, 4}.
match(b, x - 1) => match
  b => x - 1
```

### Normalization
An expression can be **normalized** to a normal form given a set of rewrite rules.

#### Example
```
Let TRS contain two rules:
  rule 1: sin(a)^2 + cos(a)^2 => one(a)
  rule 2: log(a, b) * log(b, c) => log(a, c)

normalize(log(2, sin(x)^2 + cos(x)^2 + y) * log(y + 1, z), TRS)
  => log(2, (sin(x)^2 + cos(x)^2) + y) * log(y + 1, z)        + is flat
  => log(2, 1 + y) * log(y + 1, z)                            rule 1
  => log(2, y + 1) * log(y + 1, z)                            + is orderless
  => log(2, z)                                                rule 2
```
In this example, `log(2, z)` is the normal form of `log(2, sin(x)^2 + cos(x)^2 + y) * log(y + 1, z)` given the rule set `TRS`.
