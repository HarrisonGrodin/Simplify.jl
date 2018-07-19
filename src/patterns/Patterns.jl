module Patterns

export Term, @term


abstract type Term end
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


const PROPERTIES = Dict{Symbol,Type{<:Term}}(
    :+  => Commutative{Associative},
    :++ => Associative,
    :*  => Associative,
)

Base.parse(::Type{Term}, props, t::Term) = t
Base.parse(::Type{Term}, props, n) = Constant(n)
function Base.parse(::Type{Term}, props, x::Symbol)
    x === :π && return Constant(π)
    Variable(string(x))
end
function Base.parse(::Type{Term}, props, ex::Expr)
    ex.head == :call || return Expr(ex.head, parse.(Term, props, ex.args)...)
    T = get(props, ex.args[1], Fn)
    parse(T, props, ex)
end
Base.parse(T::Type{Term}, x) = parse(T, PROPERTIES, x)


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
    else
        props = PROPERTIES
    end

    :(parse(Term, $props, $(Meta.quot(ex))))
end
macro term(ex)
    :(parse(Term, $(Meta.quot(ex))))
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

end # module
