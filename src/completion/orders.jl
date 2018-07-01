export LPO

"""
Generates lexicographic path order based on given signature order.

# Examples
```julia
>ₗₚₒ = LPO(Fn{:f,2}, Fn{:i,1}, Fn{:e,0})
```
"""
struct LPO
    order::Vector{Type{<:Term}}
end
LPO(order::Type{<:Term}...) = LPO(collect(order))

(>ₗₚₒ::LPO)(s, t) = false
(>ₗₚₒ::LPO)(s::Term, t::Variable) = occursin(t, s)

function (>ₗₚₒ::LPO)(s::F, t::G) where {F<:Fn,G<:Fn}
    s == t && return false

    any(s) do sᵢ
        sᵢ >ₗₚₒ t || sᵢ == t
    end && return true

    all(t) do tⱼ
        s >ₗₚₒ tⱼ
    end || return false

    @assert F ∈ >ₗₚₒ.order "$F is not contained in order"
    @assert G ∈ >ₗₚₒ.order "$G is not contained in order"
    findfirst(==(F), >ₗₚₒ.order) < findfirst(==(G), >ₗₚₒ.order) && return true

    if F == G
        for (sᵢ, tᵢ) ∈ zip(s, t)
            sᵢ == tᵢ && continue
            sᵢ >ₗₚₒ tᵢ && return true
        end
    end

    return false
end
