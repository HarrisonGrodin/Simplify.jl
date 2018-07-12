export unify, match


import Base: match

Base.replace(t::Term, σ::AbstractDict) = haskey(σ, t) ? replace(σ[t], σ) : map(x -> replace(x, σ), t)



struct Match <: AbstractDict{Term,Term}
    dict::Dict{Term,Term}
end
Match(ps::Pair...) = Match(Dict{Term,Term}(ps))
Base.length(σ::Match) = length(σ.dict)
Base.iterate(σ::Match) = iterate(σ.dict)
Base.iterate(σ::Match, state) = iterate(σ.dict, state)
Base.getindex(σ::Match, t::Term) = getindex(σ.dict, t)
Base.setindex!(σ::Match, value::Term, key::Term) = (setindex!(σ.dict, value, key); σ)
Base.get(σ::Match, t::Term, default) = get(σ.dict, t, default)

Base.replace(t::Term, σ::Match) = haskey(σ, t) ? σ(σ[t]) : map(σ, t)
(σ::Match)(t::Term) = replace(t, σ)

function Base.merge!(σ::Match, σs::Match...)
    for σ′ ∈ σs
        for (k, v) in σ′
            if haskey(σ, k)
                σ[k] == v || return nothing
            else
                σ[k] = v
            end
        end
    end
    σ
end
Base.merge(σ::Match, σs::Match...) = merge!(Match(), σ, σs...)

"""
    match(pattern::Term, subject::Term) -> Union{Match, Nothing}

Match term `t` to `pattern`, producing a substitution if the process succeeds.

# Examples
```jldoctest
julia> match(@term(x), @term(f(x)))
Match with 1 entry:
  @term(x) => @term(f(x))

julia> match(@term(f(x)), @term(x))

julia> match(@term(f(x, x)), @term(f(a, b)))

julia> match(@term(f(x, x)), @term(f(a, a)))
Match with 1 entry:
  @term(x) => @term(a)
```
"""
function match(pattern::Term, subject::Term) end

match(x::Variable, t::Term) = Match(x => t)
function match(f::F, g::F) where {F<:Fn}
    sub_matches = []

    for (x, y) ∈ zip(f, g)
        σ = match(x, y)
        σ === nothing && return nothing
        push!(sub_matches, σ)
    end

    merge!(Match(), sub_matches...)
end
match(a::Constant{T}, b::Constant{<:T}) where {T} =
    get(a) == get(b) ? Match() : nothing
match(::Term, ::Term) = nothing



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

Unify terms `t` and `u`, producing a substitution if the process succeeds.

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
