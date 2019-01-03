# Rewrite.jl

[![Travis Build Status](https://travis-ci.org/HarrisonGrodin/Rewrite.jl.svg?branch=master)](https://travis-ci.org/HarrisonGrodin/Rewrite.jl)
[![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/a59v394qf05c7uec/branch/master?svg=true)](https://ci.appveyor.com/project/HarrisonGrodin/rewrite-jl/branch/master)
[![Coverage Status](https://coveralls.io/repos/github/HarrisonGrodin/Rewrite.jl/badge.svg?branch=master)](https://coveralls.io/github/HarrisonGrodin/Rewrite.jl?branch=master)

**Rewrite.jl** implements methods for rewriting symbolic terms in the Julia language based on custom, domain-specific axioms and properties.

## Examples
Term rewriting can be applied to a wide variety of fields, including elementary, boolean, and abstract algebras.

Normalization involves determining the unique normal form of an expression ("simplest" equivalent expression) through repeated application of rules. *Rewrite.jl* will use its [internal set of algebraic rules](./src/rules.jl) by default, which includes trigonometry, logarithms, differentiation (based on [DiffRules.jl](https://github.com/JuliaDiff/DiffRules.jl)), and more.
```julia
julia> @syms x y b θ;

julia> normalize(@term(1 / (sin(-θ) / cos(-θ))))
@term(1 / (-(sin(θ)) / cos(θ)))

julia> normalize(@term(log(b, 1 / (b^abs(x^2)))))
@term(log(b, 1 / b ^ abs(x ^ 2)))

julia> normalize(@term(diff(sin(2x) - log(x+y), x)))
@term(1 * -(inv(x + y) * (1 * diff(y, x) + 1 * one(x))) + 1 * cos(2x) * (2 * one(x) + x * 0))

julia> normalize(@term(!x & x | (y & (y | true))))
@term(!x & x | (y | true) & y)

julia> normalize(@term(y^(6 - 3log(x, x^2))))
@term(y ^ (-(6 * log(x, x)) + 6))
```

In many cases, it is useful to specify entirely custom rules by passing a Term Rewriting System as the second argument to `normalize`. This may be done either by manually constructing a `Rules` object or by using the `RULES` strategy for `@term`.
```julia
julia> @syms f g h;
       @vars x y;

julia> normalize(@term(f(x, f(y, y))), @term RULES [
          f(x, x) => 1
          f(x, 1) => x
      ])
@term(x)

julia> normalize(@term(f(g(f(1), h()))), Rules(
          @term(f(x)) => @term(x),
          @term(h())  => @term(3),
      ))
@term(g(1, 3))

julia> using Rewrite: EvalRule

julia> normalize(@term(f(g(f(1), h()))), Rules(
          @term(f(x)) => @term(x),
          @term(h())  => @term(3),
          EvalRule(g, (a, b) -> 2a + b)
      ))
@term(5)
```

Variables may contain information about their domain, which may result in more specific normalizations.
```julia
julia> using SpecialSets

julia> @syms x y z;

julia> ctx = [Rewrite.CONTEXT; Image(y, GreaterThan(3)); Image(z, Even ∩ LessThan(0))];

julia> with_context(ctx) do
           normalize(@term(abs(x)))
       end
@term(abs(x))

julia> with_context(ctx) do
           normalize(@term(abs(y)))
       end
@term(y)

julia> with_context(ctx) do
           normalize(@term(abs(z)))
       end
@term(-z)
```

```julia
julia> ctx = [Rewrite.CONTEXT; Image(x, TypeSet(Int)); Image(y, TypeSet(Int))];

julia> with_context(ctx) do
           normalize(@term(diff(sin(2x) - log(x + y), x)))
       end
@term(cos(2x) * 2 + -(inv(x + y) * (diff(y, x) + 1)))
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
