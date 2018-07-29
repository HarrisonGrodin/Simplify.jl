export Variable, Constant, Fn, Associative, Commutative

import Base: setindex


"""
    fn_name(::Term) -> Union{Symbol, Nothing}

Return the name of a given function, or `nothing` if the argument is not a function.
"""
fn_name(::Term) = nothing


struct Variable{I<:AbstractSet} <: Term
    name::Symbol
    index::Int
    image::I
end
Variable(name, index::Int) = Variable(name, index, TypeSet(Any))
Variable(name, image::AbstractSet) = Variable(name, 0, image)
Variable(name::Symbol) = Variable(name, 0)
function Variable(name::String)
    vk = Dict(v => k for (k, v) ∈ pairs(SUBSCRIPTS))
    name = collect(name)
    i = li = lastindex(name)
    index = 0

    while !isempty(name)
        name[i] ∈ keys(vk) || break
        index += vk[name[i]] * 10^(li - i)
        i -= 1
    end

    Variable(Symbol(name[1:i]...), index)
end
Base.convert(::Type{Variable}, name::Symbol, context) = Variable(name)
Base.:(==)(x::Variable{T}, y::Variable{T}) where {T} =
    (x.name, x.index, x.image) == (y.name, y.index, y.image)
Base.string(x::Variable) = x.index == 0 ? string(x.name) : string(x.name, subscript(x.index))
Base.parse(x::Variable) = Symbol(string(x))

const SUBSCRIPTS = Dict{Int,Char}(
    0 => '₀',
    1 => '₁',
    2 => '₂',
    3 => '₃',
    4 => '₄',
    5 => '₅',
    6 => '₆',
    7 => '₇',
    8 => '₈',
    9 => '₉',
)
function subscript(x::Integer)
    result = map(reverse(digits(abs(x)))) do c
        SUBSCRIPTS[c]
    end |> join
    x < 0 ? "₋$result" : result
end


struct Constant{T} <: Term
    value::T
end
Base.convert(::Type{Constant{T}}, value, context) where {T} = Constant{T}(value)
Base.convert(::Type{Constant}, value, context) = Constant(value)
Base.get(x::Constant) = x.value
Base.parse(x::Constant) = get(x)


struct Fn <: Term
    name::Symbol
    args::Vector{Term}
    Fn(name, args...) = new(name, collect(args))
end
function Base.convert(::Type{Fn}, ex::Expr, context)
    ex.head === :call || throw(ArgumentError("Unable to convert $ex to an Fn; not a function call"))
    Fn(ex.args[1], convert.(Term, ex.args[2:end], context)...)
end
Base.:(==)(f::Fn, g::Fn) = (f.name == g.name) && (f.args == g.args)
fn_name(fn::Fn) = fn.name
Base.iterate(fn::Fn) = iterate(fn.args)
Base.iterate(fn::Fn, start) = iterate(fn.args, start)
Base.length(fn::Fn) = length(fn.args)
Base.hash(fn::Fn, h::UInt) = hash(hash((fn.name, fn.args), hash(Fn)), h)
Base.getindex(fn::Fn, key) = fn.args[key]
Base.setindex(fn::Fn, val) = length(val) == length(fn) ? Fn(fn.name, val...) :
    throw(ArgumentError("Invalid number of arguments for $(fn.name)"))
Base.setindex(fn::Fn, val, key...) = Fn(fn.name, setindex!(copy(fn.args), val, key...))
Base.map(f, fn::Fn) = setindex(fn, map(f, fn.args))
Base.parse(fn::Fn) = Expr(:call, fn.name, parse.(fn.args)...)


struct Associative <: Term
    name::Symbol
    args::Vector{Term}
    function Associative(name, args...)
        flat = flatten(name, args)
        length(flat) ≥ 2 || throw(ArgumentError("Not enough arguments to construct an Associative"))
        new(name, collect(flat))
    end
end
function Base.convert(::Type{Associative}, ex::Expr, context)
    ex.head === :call || error("LOSE FIXME")
    args = ex.args[2:end]
    length(args) == 1 && return convert(Term, args[1], context)
    Associative(ex.args[1], map(x -> convert(Term, x, context), args)...)
end
Base.:(==)(f::Associative, g::Associative) = (f.name == g.name) && (f.args == g.args)
fn_name(fn::Associative) = fn.name
Base.iterate(fn::Associative) = iterate(fn.args)
Base.iterate(fn::Associative, state) = iterate(fn.args, state)
Base.length(fn::Associative) = length(fn.args)
Base.hash(fn::Associative, h::UInt) = hash(hash((fn.name, fn.args), hash(Associative)), h)
Base.getindex(fn::Associative, inds...) = getindex(fn.args, inds...)
Base.setindex(fn::Associative, val) = Associative(fn.name, val...)
Base.setindex(fn::Associative, val, key...) = Associative(fn.name, setindex!(copy(fn.args), val, key...))
Base.map(f, fn::Associative) = setindex(fn, map(f, fn.args))
Base.parse(fn::Associative) = Expr(:call, fn.name, parse.(fn.args)...)

flatten(name, args) = [_flatten.(name, args)...;]
_flatten(name, fn::Associative) = fn.name === name ? [_flatten.(name, fn.args)...;] : fn
_flatten(name, x) = x


struct Commutative{T<:Term} <: Term
    fn::T
    function Commutative{T}(fn) where {T}
        args = collect(fn)
        new{T}(setindex(fn, sort(args, lt=_sort_lt)))
    end
end
Commutative(fn) = Commutative{typeof(fn)}(fn)
Base.convert(::Type{Commutative{T}}, ex, context) where {T} = Commutative(convert(T, ex, context))
Base.convert(::Type{Commutative}, ex, context) = convert(Commutative{Fn}, ex, context)
Base.:(==)(f::F, g::F) where {F<:Commutative} = f.fn == g.fn
Base.:(==)(::Commutative, ::Commutative) = false
fn_name(fn::Commutative) = fn_name(fn.fn)
Base.iterate(fn::Commutative) = iterate(fn.fn)
Base.iterate(fn::Commutative, state) = iterate(fn.fn, state)
Base.length(fn::Commutative) = length(fn.fn)
Base.hash(fn::Commutative, h::UInt) = hash(hash(fn.fn, hash(typeof(fn))), h)
Base.getindex(fn::Commutative, inds...) = getindex(fn.fn, inds...)
Base.setindex(fn::Commutative{T}, val, key...) where {T} = Commutative{T}(setindex(fn.fn, val, key...))
Base.map(f, fn::Commutative) = setindex(fn, map(f, fn.fn))
Base.parse(fn::Commutative) = parse(fn.fn)

# FIXME
const _order = [Commutative, Associative, Fn, Variable, Constant]
_sort_lt(f::Commutative, g::Commutative) = _sort_lt(f.fn, g.fn)
function _sort_lt(f::Associative, g::Associative)
    f.name == g.name || return f.name < g.name
    length(f) > length(g) ? true : all(p -> _sort_lt(p...), zip(f, g))
end
function _sort_lt(f::Fn, g::Fn)
    f.name == g.name || return f.name < g.name
    length(f) == length(g) || return length(f) > length(g)
    all(p -> _sort_lt(p...), zip(f, g))
end
_sort_lt(a::Constant, b::Constant) = repr(get(a)) < repr(get(b))
_sort_lt(a::Variable, b::Variable) = a.name < b.name ? true : a.index < b.index
_sort_lt(::A, ::B) where {A<:Term,B<:Term} =
    findfirst((T -> A <: T), _order) < findfirst((T -> B <: T), _order)
