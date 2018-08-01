module Patterns

using SpecialSets

export Term, @term


abstract type Term end
Base.convert(::Type{Term}, ex::Term) = ex
Base.convert(::Type{T}, ex::T) where {T<:Term} = ex
Base.getindex(t::Term, key, key′, keys...) = getindex(t[key], key′, keys...)
Base.occursin(a::Term, b::Term) = a == b || any(x -> occursin(a, x), b)
Base.length(::Term) = 0
Base.iterate(::Term) = nothing
Base.iterate(::Term, ::Any) = nothing
Base.map(f, t::Term) = t
Base.issubset(a::Term, b::Term) = !isempty(match(b, a))
Base.show(io::IO, t::Term) = print(io, "@term(", string(t), ")")
Base.string(t::Term) = string(parse(t))

Base.replace(t::Term, σ::AbstractDict) = haskey(σ, t) ? σ[t] : map(x -> replace(x, σ), t)


include("types.jl")


macro term(ex)
    :(convert(Term, $(Meta.quot(ex))))
end

_strategy(::Val{S}) where {S} = string(S)
macro term(v::Val, ex)
    strategy = _strategy(v)
    :(throw(ArgumentError("Undefined @term strategy: " * $strategy)))
end
macro term(strategy::Symbol, expr)
    esc(:(@term $(Val(strategy)) $expr))
end


include("match.jl")
include("context.jl")

end # module
