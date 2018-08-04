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
1.  Function name of expression matches function name of pattern.
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
Let a ≥ 0.
match(√(a), √(abs(-y ^ 2 - 108))) => match
  a => abs(-y ^ 2 - 108)
```
```
Let b ∈ Odd, x ∈ {2, 4}
match(b, x - 1) => match
  b => x - 1
```

#### Properties
Orderless (commutative) functions are matched without order, while flat (associative) functions are matched so that a variable in pattern can match multiple children expressions. Many functions by default have one or both of the properties. For example, `+` is by default orderless and flat, and `*` is by default flat. Properties are configurable and may be derived from the rewrite domain.
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

### Normalization
An expression can be **normalized** to a normal form given a set of rewrite rules.
##### Example
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
### Completion
In Rewrite.jl, Knuth-Bendix Completion Algorithm is used to transform a given set of axioms into a confluent rewrite system with rules. One axiom is added to the rule set each time, when the user would define the reduction order between sides of the axiom. Then, critical pairs, which are patterns that yield different normal forms when two rules are applied, are found through unification. The user is required to identify the more reduced form between the critical terms.

##### Critical Pairs Example
Suppose the rule set contains the following two rules:
```
rule 1: (x + y) + z => x + (y + z)
rule 2: x + -x => 0
rule 3: 0 + x => x
```
Given these rules, the term `(x + -x) + y` can be normalized to either `x + (-x + y)` from rule 1, or `0 + y` from rule 2 and then `y` from rule 3. In this case, the user must state which critical term is more reduced than the other. Supposing that `y` is more reduced than `x + (-x + y)`, a new rule is generated:
```
rule 4: x + (-x + y) => y
```

Once all axioms have been transformed and added to the rule set, the rule set is called **complete** and ready for rewriting expressions. According to Knuth-Bendix Completion Algorithm, if the rule set is confluent and terminating, every expression can be rewritten to a unique normal form.

