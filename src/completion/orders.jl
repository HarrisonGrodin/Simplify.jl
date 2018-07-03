export LexicographicPathOrder, LPO


abstract type AbstractOrder end

"""
Generates lexicographic path order based on given signature order.

# Examples
```jldoctest
julia> >ₗₚₒ = LPO(Fn{:f,2}, Fn{:i,1}, Fn{:e,0});
```
"""
struct LexicographicPathOrder <: AbstractOrder
    order::Vector{Type}
end
LexicographicPathOrder(order::Type...) = LexicographicPathOrder(collect(order))
Base.in(t::Type, lpo::LexicographicPathOrder) = t ∈ lpo.order
const LPO = LexicographicPathOrder

(>ₗₚₒ::LPO)(s, t) = false
(>ₗₚₒ::LPO)(s::Term, t::Variable) = occursin(t, s)

function (>ₗₚₒ::LPO)(s::F, t::G) where {F<:Fn,G<:Fn}
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
