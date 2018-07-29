module Patterns

using SpecialSets

export Term, @term


abstract type Term end
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


const PROPERTY_NAMES = Dict{Symbol,Type{<:Term}}(
    :A           => Associative,
    :Assoc       => Associative,
    :Associative => Associative,
    :C           => Commutative,
    :Comm        => Commutative,
    :Commutative => Commutative,
    :AC          => Commutative{Associative},
    :AssociativeCommutative => Commutative{Associative},
)

macro term(ex::Expr)
    if ex.head === :where
        ex, _props = ex.args[1], ex.args[2:end]
        props = Dict{Symbol,Type}()
        for prop ∈ _props
            (prop isa Expr && prop.head == :(::)) || (@warn "Invalid property '$prop'; ignoring"; continue)
            f, p = prop.args
            haskey(PROPERTY_NAMES, p) || (@warn "Unknown property '$p'; ignoring"; continue)
            props[f] = PROPERTY_NAMES[p]
        end
        props = AlgebraContext(props, Dict())
        return :(convert(Term, $(Meta.quot(ex)), $props))
    end

    :(convert(Term, $(Meta.quot(ex))))
end
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
