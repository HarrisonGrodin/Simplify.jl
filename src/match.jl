import Base: match
using Combinatorics: combinations, permutations

export match, Match


struct Match <: AbstractDict{Variable,Any}
    match::Dict{Variable,Any}
end
Match(xs::Pair...) = Match(Dict(xs))
Base.replace(t::Term, σ::AbstractDict) =
    haskey(σ, get(t)) ? Term(σ[get(t)]) : map(x -> replace(x, σ), t)
(σ::Match)(t::Term) = replace(t, σ)

Base.length(σ::Match) = length(σ.match)
Base.iterate(σ::Match) = iterate(σ.match)
Base.iterate(σ::Match, state) = iterate(σ.match, state)
Base.get(σ::Match, x::Variable, default) = get(σ.match, x, default)
Base.keys(σ::Match) = keys(σ.match)
Base.getindex(σ::Match, keys...) = getindex(σ.match, keys...)
Base.setindex!(σ::Match, val, keys...) = setindex!(σ.match, val, keys...)

function Base.merge!(σ::Match, σ′::Match)
    for (k, v) ∈ σ′
        if haskey(σ, k)
            σ[k] == v || return nothing
        else
            σ[k] = v
        end
    end
    σ
end
Base.merge!(::Nothing, ::Match) = nothing
Base.merge!(σ::Match, σs::Match...) = foldl(merge!, σs; init=σ)

Base.merge(σ::Match, σs::Match...) = merge!(Match(), σ, σs...)


"""
    match(pattern::Term, subject::Term) -> σ::Union{Match,Nothing}

Match term `t` to `pattern`, producing a `Match` if the process succeeds.

# Examples
```jldoctest
julia> @vars x;

julia> @syms a b f;

julia> match(@term(x), @term(f(a)))
Rewrite.Match with 1 entry:
  x => :((f)(a))

julia> match(@term(f(x)), @term(a))

julia> match(@term(f(x, x)), @term(f(a, b)))

julia> match(@term(f(x, x)), @term(f(a, a)))
Rewrite.Match with 1 entry:
  x => a
```
"""
match(pattern::Term, subject::Term) = match(Term, get(pattern), get(subject))

match(::Type{Term}, x::Variable, t) = Match(x => t)
function match(::Type{Term}, p::Expr, s::Expr)
    p.head === s.head                || return nothing
    length(p.args) == length(s.args) || return nothing

    σ = Match()

    for (x, y) ∈ zip(p.args, s.args)
        σ′ = match(Term, x, y)
        σ′ === nothing && return nothing
        σ = merge!(σ, σ′)
    end

    σ
end
match(::Type{Term}, p, s) = (typeof(s) <: typeof(p) && p == s) ? Match() : nothing
