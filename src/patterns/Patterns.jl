module Patterns

using StaticArrays


abstract type AbstractExpr end

const Substitution = Dict{AbstractExpr,AbstractExpr}

Base.match(::AbstractExpr, ::AbstractExpr) = nothing
Base.issubset(a::AbstractExpr, b::AbstractExpr) = match(b, a) !== nothing
Base.replace(ex::AbstractExpr, substitution) = ex


include("types.jl")
include("io.jl")

end # module
