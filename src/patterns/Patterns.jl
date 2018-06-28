module Patterns

export Term, @term, Substitution


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
Base.parse(::Type{Term}, x::Symbol) = Variable(string(x))
function Base.parse(::Type{Term}, ex::Expr)
    ex.head == :$    && return :(parse(Term, $(esc(ex.args[1]))))
    ex.head == :call || return Expr(ex.head, parse.(Term, ex.args)...)
    :(Fn{$(Meta.quot(ex.args[1]))}($(parse.(Term, ex.args[2:end])...)))
end

macro term(ex)
    parse(Term, ex)
end
macro term(strategy, ex)
    if strategy == :PAIRS
        @assert ex.head == :vect
        args = map(ex.args) do pair
            p, a, b = pair.args
            @assert p == :(=>)
            :(Pair($(parse(Term, a)), $(parse(Term, b))))
        end
        return :(Pair[$(args...)])
    end
    throw(ArgumentError("Unknown @term strategy: $strategy"))
end


const Substitution = Dict{Variable,Term}
Base.getindex(σ::Substitution, x::Variable) = get(σ, x, x)
Base.:∘(σs::Substitution...) = merge(σs...)
(σ::Substitution)(x::Variable) = σ[x]
(σ::Substitution)(xs) = map(σ, xs)
(σ::Substitution)(σ′::Substitution) = Substitution(a => σ(b) for (a, b) ∈ pairs(σ′))
Base.replace(t::Term, σ::Substitution) = σ(t)

include("unify.jl")

end # module
