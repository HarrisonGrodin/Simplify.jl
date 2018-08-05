export LexicographicPathOrder, LPO


abstract type AbstractOrder end

"""
Generates lexicographic path order based on given signature order.

# Examples
```jldoctest
julia> >ₗₚₒ = LPO((:i, 1), (:f, 2), (:e, 0));
```
"""
struct LexicographicPathOrder <: AbstractOrder
    order::Vector{Tuple{Symbol,Int}}
end
LexicographicPathOrder(order::Tuple...) = LexicographicPathOrder(collect(order))
Base.in(t, lpo::LexicographicPathOrder) = t ∈ lpo.order
const LPO = LexicographicPathOrder

(>ₗₚₒ::LPO)(s, t) = false
(>ₗₚₒ::LPO)(s::Term, t::Variable) = occursin(t, s)

function (>ₗₚₒ::LPO)(s::Fn, t::Fn)
    F, G = (s.name, length(s)), (t.name, length(t))
    F ∈ >ₗₚₒ || throw(ArgumentError("$F is not contained in order"))
    G ∈ >ₗₚₒ || throw(ArgumentError("$G is not contained in order"))

    s == t && return false

    any(s) do sᵢ
        sᵢ >ₗₚₒ t || sᵢ == t
    end && return true

    all(t) do tⱼ
        s >ₗₚₒ tⱼ
    end || return false

    findfirst(==(F), >ₗₚₒ.order) < findfirst(==(G), >ₗₚₒ.order) && return true

    if F == G
        for (sᵢ, tᵢ) ∈ zip(s, t)
            sᵢ == tᵢ && continue
            sᵢ >ₗₚₒ tᵢ && return true
        end
    end

    return false
end
