module Patterns

export Term, @term


abstract type Term end
Base.getindex(t::Term, key, key′, keys...) = getindex(t[key], key′, keys...)
Base.occursin(a::Term, b::Term) = a == b || any(x -> occursin(a, x), b)
Base.iterate(::Term) = nothing
Base.iterate(::Term, ::Any) = nothing
Base.map(f, t::Term) = t
Base.issubset(a::Term, b::Term) = match(b, a) !== nothing
Base.show(io::IO, t::Term) = print(io, "@term(", parse(t), ")")


include("types.jl")


Base.parse(::Type{Term}, t::Term) = t
Base.parse(::Type{Term}, n) = Constant(n)
function Base.parse(::Type{Term}, x::Symbol)
    x === :π && return Constant(π)
    Variable(string(x))
end
function Base.parse(::Type{Term}, ex::Expr)
    ex.head == :$    && return :(parse(Term, $(esc(ex.args[1]))))
    ex.head == :call || return Expr(ex.head, parse.(Term, ex.args)...)
    :(Fn{$(Meta.quot(ex.args[1]))}($(parse.(Term, ex.args[2:end])...)))
end

macro term(ex)
    parse(Term, ex)
end

_strategy(::Val{S}) where {S} = string(S)
macro term(v::Val, ex)
    strategy = _strategy(v)
    :(throw(ArgumentError("Undefined @term strategy: " * $strategy)))
end
macro term(strategy::Symbol, expr)
    esc(:(@term $(Val(strategy)) $expr))
end



include("unify.jl")

end # module
