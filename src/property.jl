export Property
export Flat, Orderless


abstract type Property end

struct Flat <: Property end
struct Orderless <: Property
    name::Symbol
    orderless::Vector{Term}
    ordered::Vector{Term}
end


property(::Type{<:Property}, x) = nothing
hasproperty(P::Type{<:Property}, x) = property(P, x) !== nothing
