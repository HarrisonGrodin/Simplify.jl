export Term
export Variable, Constant, Fn
export @term, @syms


macro term(ex)
    :(convert(Term, $(esc(_term(ex)))))
end
_term(ex::Expr) = Expr(:call, Expr, Meta.quot(ex.head), _term.(ex.args)...)
_term(x) = x


struct Term{T}
    ex::T
end
(f::Term)(xs...) = convert(Term, Expr(:call, get(f), xs...))
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
Base.convert(::Type{Symbolic}, x::Term{Symbolic}) = x.ex
Base.show(io::IO, x::Symbolic) = print(io, x.x)
macro syms(xs::Symbol...)
    sets = (:($x = $(Term(Symbolic(x)))) for x ∈ xs)
    results = Expr(:tuple, xs...)
    esc(Expr(:block, sets..., results))
end


Base.:(==)(a::Term, b::Term) = a.ex == b.ex
Base.hash(t::Term, h::UInt) = hash(t.ex, hash(Term, h))
Base.eltype(::Term) = Term
Base.get(t::Term) = t.ex
Base.length(t::Term{Expr}) = length(t.ex.args)
Base.length(::Term) = 0
Base.getindex(t::Term{Expr}, i::Number) = Term(t.ex.args[i])
Base.getindex(t::Term{Expr}, is::AbstractArray) = Term.(t.ex.args[is])
Base.getindex(t::Term, i, j, ks...) = getindex(t[i], j, ks...)
Base.lastindex(t::Term) = lastindex(t.ex.args)
function Base.iterate(t::Term{Expr})
    iter = iterate(t.ex.args)
    iter === nothing && return
    t′, state′ = iter
    (Term(t′), state′)
end
function Base.iterate(t::Term{Expr}, state)
    iter = iterate(t.ex.args, state)
    iter === nothing && return
    t′, state′ = iter
    (Term(t′), state′)
end
Base.iterate(::Term, state = nothing) = nothing
Base.occursin(a::Term, b::Term) = a == b || any(x -> occursin(a, x), b)


const Variable = Term{Symbol}
const Fn = Term{Expr}

function Base.map(f, t::Fn)
    args = map(traverse ∘ f, collect(t))
    convert(Term, Expr(t.ex.head, args...))
end
Base.map(f, t::Term) = t

Base.issubset(a::Term, b::Term) = !isempty(match(b, a))
Base.show(io::IO, t::Term) = print(io, "@term(", get(t), ")")
Base.replace(t::Fn, σ::AbstractDict) = haskey(σ, t) ? σ[t] : map(x -> replace(x, σ), t)
Base.replace(t::Term, σ::AbstractDict) = haskey(σ, t) ? σ[t] : t
