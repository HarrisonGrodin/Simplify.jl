export unify, match


"""
    overlay(pattern, term) -> Union{Nothing, Tuple{Substitution,Any}}

Attempt to match `term` to `pattern`. If the process fails, `nothing`
should be returned, signaling the complete failure of matching or unification
on the given superterms. Otherwise, a 2-tuple of the new substitution and
the generated dependency overlaying problems should be returned.
"""
function overlay end

overlay(x::Variable, t::Term) = (Substitution(x => t), ())
overlay(f::F, g::F) where {F<:Fn} = (Substitution(), zip(f, g))
overlay(a::Constant, b::Constant) = get(a) == get(b) ? (Substitution(), ()) : nothing
overlay(::Term, ::Term) = nothing


_unify(σ::Substitution) = σ
function _unify(σ::Substitution, (a, b), ms...)
    res = overlay(a, b)
    res === nothing && (res = overlay(b, a))
    res === nothing && return nothing
    (σ′, ms′) = res

    σ′ = filter(σ′) do (x, t)
        x ≠ t
    end
    any(σ′) do (x, t)
        occursin(x, t)
    end && return nothing

    _unify(σ′ ∘ σ′(σ), ms′..., σ′.(ms)...)
end
_unify(ms...) = _unify(Substitution(), ms...)
"""
    unify(t, u) -> Union{Nothing, Substitution}

Unify terms `t` and `u`, producing a substitution if the process succeeds.

# Examples
```jldoctest
julia> unify(@term(f(y)), @term(x))
Dict{Variable,Term} with 1 entry:
  @term(x) => @term(f(y))

julia> unify(@term(x), @term(x))
Dict{Variable,Term} with 0 entries

julia> unify(@term(x), @term(f(x)))

julia> unify(@term(f(x)), @term(g(x)))

julia> unify(@term(f(x, y)), @term(f(y, z)))
Dict{Variable,Term} with 2 entries:
  @term(x) => @term(z)
  @term(y) => @term(z)
```
"""
unify(t::Term, u::Term) = _unify((t, u))


_match(σ::Substitution) = σ
function _match(σ::Substitution, (a, b), ms...)
    res = overlay(a, b)
    res === nothing && return nothing
    (σ′, ms′) = res

    for (k, v) in σ′
        if haskey(σ, k)
            σ[k] == v || return nothing
        else
            σ[k] = v
        end
    end

    _match(σ, ms′..., ms...)
end
_match(ms...) = _match(Substitution(), ms...)

"""
    match(pattern::Term, t::Term) -> Union{Nothing, Substitution}

Match term `t` to `pattern`, producing a substitution if the process succeeds.

# Examples
```jldoctest
julia> match(@term(x), @term(f(x)))
Dict{Variable,Term} with 1 entry:
  @term(x) => @term(f(x))

julia> match(@term(f(x)), @term(x))

julia> match(@term(f(x, x)), @term(f(a, b)))

julia> match(@term(f(x, x)), @term(f(a, a)))
Dict{Variable,Term} with 1 entry:
  @term(x) => @term(a)
```
"""
Base.match(pattern::Term, t::Term) = _match((pattern, t))
