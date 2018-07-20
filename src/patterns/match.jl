export unify, match


import Base: match
using Combinatorics: permutations


abstract type AbstractContext end

struct AlgebraContext <: AbstractContext
    props::Dict{Symbol,Type}
    consts::Dict{Symbol,Any}
end
function (ctx::AlgebraContext)(ex::Expr)
    ex.head === :call || throw(ArgumentError("Invalid expression head: $(repr(ex.head))"))
    typ = get(ctx.props, ex.args[1], Fn)

    convert(typ, ex, ctx)
end
(ctx::AlgebraContext)(x::Symbol) = haskey(ctx.consts, x) ? convert(Constant, ctx.consts[x]) : convert(Variable, x)
(ctx::AlgebraContext)(x) = convert(Constant, x)

Base.convert(::Type{T}, ex) where {T<:Term} = convert(T, ex, AlgebraContext(
    Dict(
        :+  => Commutative{Associative},
        :++ => Associative,
        :*  => Associative,
    ),
    Dict(
        :π  => π,
    )
))


mutable struct Match <: AbstractSet{AbstractDict{Term,Term}}
    matches::Set{Dict{Term,Term}}
end
Match(xs::Union{Pair,Dict}...) = Match(Set(Dict.(xs)))
Base.zero(::Type{Match}) = Match()
Base.one(::Type{Match}) = Match(Dict())
Base.length(Θ::Match) = length(Θ.matches)
Base.iterate(Θ::Match) = iterate(Θ.matches)
Base.iterate(Θ::Match, state) = iterate(Θ.matches, state)
Base.push!(Θ::Match, items...) = (push!(Θ.matches, items...); Θ)
Base.copy(Θ::Match) = Match(copy(Θ.matches))
Base.union(Θ₁::Match, Θ₂::Match) = Match(union(Θ₁.matches, Θ₂.matches))
(Θ::Match)(t::Term) = Set{Term}([replace(t, σ) for σ ∈ Θ])

function Base.merge!(Θ::Match, Θs::Match...)
    for Θ′ ∈ Θs
        result = Match()
        for σ′ ∈ Θ′
            foreach(Θ) do σ
                res = copy(σ)
                for (k, v) in σ′
                    if haskey(σ, k)
                        σ[k] == v || return
                    end
                    res[k] = v
                end
                push!(result, res)
            end
        end
        Θ.matches = result
    end
    Θ
end
Base.merge(σ::Match, σs::Match...) = merge!(one(Match), σ, σs...)

"""
    match(pattern::Term, subject::Term, [Θ::Match]) -> Match

Match term `t` to `pattern`, producing a `Match` if the process succeeds. If an existing
match `Θ` is provided, it is used as the base for the returned match.

# Examples
```jldoctest
julia> match(@term(x), @term(f(x)))
Match(Set(Dict{Term,Term}[Dict(@term(x)=>@term(f(x)))]))

julia> match(@term(f(x)), @term(x))
Match(Set(Dict{Term,Term}[]))

julia> match(@term(f(x, x)), @term(f(a, b)))
Match(Set(Dict{Term,Term}[]))

julia> match(@term(f(x, x)), @term(f(a, a)))
Match(Set(Dict{Term,Term}[Dict(@term(x)=>@term(a))]))
```
"""
match(pattern::Term, subject::Term) = match(pattern, subject, one(Match))
match(p::Term, s::Term, Θ, f) = match(p, s, Θ)

match(x::Variable, t::Term, Θ) = merge(Θ, Match(x => t))
match(a::Constant{T}, b::Constant{<:T}, Θ) where {T} =
    get(a) == get(b) ? Θ : zero(Match)
function match(f::F, g::F, Θ) where {F<:Fn}
    for (x, y) ∈ zip(f, g)
        Θ = match(x, y, Θ)
    end
    Θ
end
"""
    match(p::F, s::F, Θ::Match) where {F<:Associative} -> Match

Match an associative function call to another associative function call, based on the
algorithm by [Krebber](https://arxiv.org/abs/1705.00907).
"""
function match(p::F, s::F, Θ, f = F) where {F<:Associative}
    m, n = length(p), length(s)
    m > n && return zero(Match)
    n_free = n - m
    n_vars = count(x -> x isa Variable, p)
    Θᵣ = zero(Match)

    for k ∈ Iterators.product((0:n_free for i ∈ 1:n_vars)...)
        (isempty(k) ? 0 : sum(k)) == n_free || continue  # FIXME: efficiency
        i, j = 1, 1
        Θ′ = Θ
        for pₗ ∈ p
            l_sub = 0
            if pₗ isa Variable
                l_sub += k[j]
                j += 1
            end
            s′ = f(s[i:i+l_sub]...)
            Θ′ = match(pₗ, s′, Θ′)
            isempty(Θ′) && break
            i += l_sub + 1
        end
        Θᵣ = Θᵣ ∪ Θ′
    end
    Θᵣ
end
function match(p::F, s::F, Θ) where {T,F<:Commutative{T}}
    map(permutations(s)) do perm  # FIXME: efficiency
        s_fn = T(perm...)
        match(p.fn, s_fn, Θ, F ∘ T)
    end |> Base.splat(union)
end
match(::Term, ::Term, Θ) = zero(Match)



struct Unify <: AbstractDict{Term,Term}
    dict::Dict{Term,Term}
end
Unify(ps::Pair...) = Unify(Dict{Term,Term}(ps))
Base.length(σ::Unify) = length(σ.dict)
Base.iterate(σ::Unify) = iterate(σ.dict)
Base.iterate(σ::Unify, state) = iterate(σ.dict, state)
Base.getindex(σ::Unify, t::Term) = getindex(σ.dict, t)
function Base.setindex!(σ::Unify, value::Term, key::Term)
    value == key && return σ

    occursin(key, value) && return nothing
    σ.dict[key] = value

    for (k, v) ∈ pairs(σ.dict)
        σ.dict[k] = σ(v)
    end

    σ
end
Base.get(σ::Unify, t::Term, default) = get(σ.dict, t, default)

(σ::Unify)(t::Term) = replace(t, σ)
(σ::Unify)(xs) = map(σ, xs)

function Base.merge!(σ::Unify, σs::Unify...)
    for σ′ ∈ σs
        for (k, v) in σ′
            if haskey(σ, k)
                σ[k] == v || return nothing
            else
                setindex!(σ, v, k) === nothing && return nothing
            end
        end
    end
    σ
end
Base.merge(σ::Unify, σs::Unify...) = merge!(Unify(), σ, σs...)


"""
    unify(t, u) -> Union{Unify, Nothing}

Unify terms `t` and `u`, producing a `Unify` if the process succeeds.

# Examples
```jldoctest
julia> unify(@term(f(y)), @term(x))
Unify with 1 entry:
  @term(x) => @term(f(y))

julia> unify(@term(x), @term(x))
Unify with 0 entries

julia> unify(@term(x), @term(f(x)))

julia> unify(@term(f(x)), @term(g(x)))

julia> unify(@term(f(x, y)), @term(f(y, z)))
Unify with 2 entries:
  @term(x) => @term(z)
  @term(y) => @term(z)
```
"""
function unify(t::Term, u::Term) end
unify(t::Term, u::Term) = _unify((t, u))


_unify(σ::Unify, (x, y)::Tuple{Variable,Variable}, ms...) =
    x == y ? _unify(σ, ms...) : eliminate!(σ, (x, y), ms)
_unify(σ::Unify, (x, t)::Tuple{Variable,Term}, ms...) = eliminate!(σ, (x, t), ms)
_unify(σ::Unify, (t, x)::Tuple{Term,Variable}, ms...) = eliminate!(σ, (x, t), ms)
_unify(σ::Unify, (f, g)::Tuple{F,F}, ms...) where {F<:Fn} = _unify(σ, zip(f, g)..., ms...)
_unify(σ::Unify, (a, b)::Tuple{T,T}, ms...) where {T<:Constant} =
    get(a) == get(b) ? _unify(σ, ms...) : nothing
_unify(σ::Unify, ms...) = nothing
_unify(σ::Unify) = σ
_unify(ms...) = _unify(Unify(), ms...)

function eliminate!(σ::Unify, (x, t)::Tuple{Variable,Term}, ms)
    setindex!(σ, t, x) === nothing && return nothing
    _unify(σ, Unify(x => t).(ms)...)
end
