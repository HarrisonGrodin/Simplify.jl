using Rewrite

export unify, Unifier


struct Unifier <: AbstractDict{Term,Term}
    dict::Dict{Term,Term}
end
Unifier(ps::Pair...) = Unifier(Dict{Term,Term}(ps))
Base.length(σ::Unifier) = length(σ.dict)
Base.iterate(σ::Unifier) = iterate(σ.dict)
Base.iterate(σ::Unifier, state) = iterate(σ.dict, state)
Base.getindex(σ::Unifier, t::Term) = getindex(σ.dict, t)
function Base.setindex!(σ::Unifier, value::Term, key::Term)
    value == key && return σ

    occursin(key, value) && return nothing
    σ.dict[key] = value

    for (k, v) ∈ pairs(σ.dict)
        σ.dict[k] = σ(v)
    end

    σ
end
Base.get(σ::Unifier, t::Term, default) = get(σ.dict, t, default)

(σ::Unifier)(t::Term) = replace(t, σ)
(σ::Unifier)(xs) = map(σ, xs)

function Base.merge!(σ::Unifier, σs::Unifier...)
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
Base.merge(σ::Unifier, σs::Unifier...) = merge!(Unifier(), σ, σs...)


"""
    unify(t, u) -> Union{Unifier, Nothing}

Unifier terms `t` and `u`, producing a `Unifier` if the process succeeds.

# Examples
```jldoctest
julia> unify(@term(f(y)), @term(x))
Unifier with 1 entry:
  @term(x) => @term(f(y))

julia> unify(@term(x), @term(x))
Unifier with 0 entries

julia> unify(@term(x), @term(f(x)))

julia> unify(@term(f(x)), @term(g(x)))

julia> unify(@term(f(x, y)), @term(f(y, z)))
Unifier with 2 entries:
  @term(x) => @term(z)
  @term(y) => @term(z)
```
"""
unify(t::Term, u::Term) = _unify((t, u))


_unify(σ::Unifier, (x, y)::Tuple{Variable,Variable}, ms...) =
    x == y ? _unify(σ, ms...) : eliminate!(σ, (x, y), ms)
_unify(σ::Unifier, (x, t)::Tuple{Variable,Term}, ms...) = eliminate!(σ, (x, t), ms)
_unify(σ::Unifier, (t, x)::Tuple{Term,Variable}, ms...) = eliminate!(σ, (x, t), ms)
_unify(σ::Unifier, (f, g)::Tuple{Fn,Fn}, ms...) =
    f.name == g.name && length(f) == length(g) ? _unify(σ, zip(f, g)..., ms...) : nothing
_unify(σ::Unifier, (a, b)::Tuple{T,T}, ms...) where {T<:Constant} =
    get(a) == get(b) ? _unify(σ, ms...) : nothing
_unify(σ::Unifier, ms...) = nothing
_unify(σ::Unifier) = σ
_unify(ms...) = _unify(Unifier(), ms...)

function eliminate!(σ::Unifier, (x, t)::Tuple{Variable,Term}, ms)
    setindex!(σ, t, x) === nothing && return nothing
    _unify(σ, Unifier(x => t).(ms)...)
end
