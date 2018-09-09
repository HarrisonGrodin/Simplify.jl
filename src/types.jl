export Term,     @term
export Symbolic, @syms
export Variable, @vars


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

Base.:(==)(a::Term, b::Term) = a.ex == b.ex
Base.hash(t::Term, h::UInt) = hash(t.ex, hash(Term, h))
Base.eltype(::Term) = Term
Base.get(t::Term) = t.ex
Base.occursin(a::Term, b::Term) = a == b || any(x -> occursin(a, x), b)

Base.iterate(t::Term) = _iterate(get(t))
Base.iterate(t::Term, state) = _iterate(get(t), state)
_iterate(ex::Expr) = (Term(first(ex.args)), 1)
function _iterate(ex::Expr, state)
    state > lastindex(ex.args) && return
    (Term(ex.args[state]), state + 1)
end
_iterate(x, state = nothing) = nothing

Base.map(f, t::Term) = convert(Term, _map(f, get(t)))
_map(f, ex::Expr) = Expr(ex.head, map(f ∘ Term, ex.args)...)
_map(f, x) = x

Base.issubset(a::Term, b::Term) = !isempty(match(b, a))
function Base.show(io::IO, t::Term)
    repr = sprint(show, Expr(:macrocall, Symbol("@term"), nothing, get(t)))[9:end-1]
    print(io, "@term(", repr, ")")
end

Base.replace(t::Term, σ) = haskey(σ, get(t)) ? Term(σ[get(t)]) : map(x -> replace(x, σ), t)


struct Symbolic
    x::Symbol
end
(x::Symbolic)(xs...) = Expr(:call, x, xs...)
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
