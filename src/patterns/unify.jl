export unify, match


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
Base.match(pattern::Term, t::Term) = _match((pattern, t))
