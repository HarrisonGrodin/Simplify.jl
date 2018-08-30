export Property
export Flat, Orderless


abstract type Property end

struct Fn2 <: Property  # Rename to Fn once name clashes solved
    name::Any
    args::Vector{Any}
end
Base.getindex(fn::Fn2, inds...) = getindex(fn.args, inds...)

struct Flat <: Property
    name::Any
    args::Vector{Any}
end
Base.getindex(p::Flat, inds...) = getindex(p.args, inds...)

struct Orderless <: Property
    name::Symbol
    orderless::Vector{Term}
    ordered::Vector{Term}
end


property(::Type{<:Property}, x) = nothing
property(P::Type{<:Property}, t::Term) = property(P, get(t))
hasproperty(P::Type{<:Property}, x) = property(P, x) !== nothing
