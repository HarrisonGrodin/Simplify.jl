export Term,     @term
export Symbolic, @syms
export Variable, @vars

using MacroTools

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
    ex1 = Expr(:macrocall, Symbol("@term"), nothing, _quote(get(t)))
    ex = MacroTools.postwalk(x -> x isa Function ? nameof(x) : x, ex1)
    repr = sprint(show, ex)[9:end-1]
    print(io, "@term(", repr, ")")
end
_quote(x::Symbol) = Meta.quot(x)
_quote(ex::Expr) = Expr(ex.head, _quote.(ex.args)...)
_quote(x) = x

Base.replace(t::Term, σ) = haskey(σ, get(t)) ? Term(σ[get(t)]) : map(x -> replace(x, σ), t)


struct Symbolic
    name::Symbol
end
(x::Symbolic)(xs...) = Expr(:call, x, xs...)
Base.show(io::IO, x::Symbolic) = print(io, x.name)
macro syms(xs::Symbol...)
    syms = (:($x = $(Symbolic(x))) for x ∈ xs)
    results = Expr(:tuple, xs...)
    esc(Expr(:block, syms..., results))
end


struct Variable
    name::Symbol
end
Base.show(io::IO, x::Variable) = print(io, x.name)
macro vars(xs::Symbol...)
    vars = (:($x = $(Variable(x))) for x ∈ xs)
    results = Expr(:tuple, xs...)
    esc(Expr(:block, vars..., results))
end
