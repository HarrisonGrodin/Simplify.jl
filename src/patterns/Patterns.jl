module Patterns

export Term


abstract type Term end
Base.getindex(t::Term, key...) = foldl(getindex, t, key)
Base.occursin(a::Term, b::Term) = a == b || any(x -> occursin(a, x), b)
Base.iterate(::Term) = nothing
Base.iterate(::Term, ::Any) = nothing

Base.issubset(a::Term, b::Term) = match(b, a) !== nothing
Base.replace(ex::Term, substitution) = ex


include("types.jl")
include("io.jl")

end # module
