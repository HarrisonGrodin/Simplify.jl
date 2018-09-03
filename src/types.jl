export Term
export Variable, Fn
export @term, @syms, @vars


macro term(ex)
    :(convert(Term, $(esc(_term(ex)))))
end
function _term(ex::Expr)
    ex.head === :$ && return ex.args[1]
    Expr(:call, Expr, Meta.quot(ex.head), _term.(ex.args)...)
end
_term(x) = x


struct Term
    ex
end
(f::Term)(xs...) = convert(Term, Expr(:call, f, xs...))
Base.convert(::Type{Term}, ex::Term) = ex
Base.convert(::Type{Term}, ex) = Term(traverse(ex))
traverse(t::Term) = get(t)
traverse(ex::Expr) = Expr(ex.head, traverse.(ex.args)...)
traverse(x) = x


struct Symbolic
    x::Symbol
end
Base.convert(::Type{Symbol}, x::Symbolic) = x.x  # FIXME
Base.convert(::Type{Symbolic}, x::Symbol) = Symbolic(x)
Base.convert(::Type{Symbolic}, x::Symbolic) = x
Base.show(io::IO, x::Symbolic) = print(io, x.x)
macro syms(xs::Symbol...)
    syms = (:($x = $(Symbolic(x))) for x ∈ xs)
    results = Expr(:tuple, xs...)
    esc(Expr(:block, syms..., results))
end


mutable struct Variable
    image::AbstractSet
end
Variable() = Variable(TypeSet(Any))
Base.show(io::IO, x::Variable) = print(io, "#=VAR@$(objectid(x))=#")
macro vars(xs::Symbol...)
    vars = (:($x = Variable()) for x ∈ xs)
    results = Expr(:tuple, xs...)
    esc(Expr(:block, vars..., results))
end


Base.:(==)(a::Term, b::Term) = a.ex == b.ex
Base.hash(t::Term, h::UInt) = hash(t.ex, hash(Term, h))
Base.eltype(::Term) = Term
Base.get(t::Term) = t.ex
Base.iterate(::Term, state = nothing) = nothing
Base.occursin(a::Term, b::Term) = a == b || any(x -> occursin(a, x), b)


Base.map(f, t::Term) = convert(Term, _map(f, get(t)))
_map(f, ex::Expr) = Expr(ex.head, map(f ∘ Term, ex.args)...)
_map(f, x) = x

Base.issubset(a::Term, b::Term) = !isempty(match(b, a))
Base.show(io::IO, t::Term) = print(io, "@term(", get(t), ")")

Base.replace(t::Term, σ) = haskey(σ, t) ? σ[t] : map(x -> replace(x, σ), t)
