module Patterns

export Term, Substitution


abstract type Term end
Base.getindex(t::Term, key...) = foldl(getindex, t, key)
Base.occursin(a::Term, b::Term) = a == b || any(x -> occursin(a, x), b)
Base.iterate(::Term) = nothing
Base.iterate(::Term, ::Any) = nothing
Base.map(f, t::Term) = t
Base.issubset(a::Term, b::Term) = match(b, a) !== nothing


include("types.jl")
include("io.jl")


const Substitution = Dict{Variable,Term}
Base.getindex(σ::Substitution, x::Variable) = get(σ, x, x)
Base.:∘(σs::Substitution...) = merge(σs...)
(σ::Substitution)(x::Variable) = σ[x]
(σ::Substitution)(xs) = map(σ, xs)
(σ::Substitution)(σ′::Substitution) = Substitution(a => σ(b) for (a, b) ∈ pairs(σ′))
Base.replace(t::Term, σ::Substitution) = σ(t)

include("unify.jl")

end # module
