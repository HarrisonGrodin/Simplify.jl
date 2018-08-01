export Variable, Constant, Fn

import Base: setindex


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
Base.convert(::Type{Variable}, name::Symbol) = Variable(name)
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
Base.convert(::Type{Constant{T}}, value::Constant{T}) where {T} = value
Base.convert(::Type{Constant{T}}, value) where {T} = Constant{T}(value)
Base.convert(::Type{Constant}, value::Constant) = value
Base.convert(::Type{Constant}, value) = Constant(value)
Base.get(x::Constant) = x.value
Base.parse(x::Constant) = get(x)


struct Fn <: Term
    name::Symbol
    args::Vector{Term}
    function Fn(name, args...; clean=true)
        fn = new(name, collect(args))

        if clean
            hasproperty(Flat, fn) && flatten!(fn)
            hasproperty(Orderless, fn) && sort!(fn)
        end

        fn
    end
end
function Base.convert(::Type{Fn}, ex::Expr)
    ex.head === :call || throw(ArgumentError("Unable to convert $ex to an Fn; not a function call"))
    Fn(ex.args[1], convert.(Term, ex.args[2:end])...)
end
Base.:(==)(f::Fn, g::Fn) = (f.name == g.name) && (f.args == g.args)
Base.iterate(fn::Fn) = iterate(fn.args)
Base.iterate(fn::Fn, start) = iterate(fn.args, start)
Base.length(fn::Fn) = length(fn.args)
Base.hash(fn::Fn, h::UInt) = hash(hash((fn.name, fn.args), hash(Fn)), h)
Base.getindex(fn::Fn, key) = fn.args[key]
Base.setindex(fn::Fn, val, key...) = Fn(fn.name, setindex!(copy(fn.args), val, key...))
Base.map(f, fn::Fn) = Fn(fn.name, map(f, fn.args)...)
Base.parse(fn::Fn) = Expr(:call, fn.name, parse.(fn.args)...)


function flatten!(fn::Fn)
    flat = flatten!(fn.name, fn)
    append!(empty!(fn.args), flat)
    fn
end
flatten!(name, fn::Fn) = fn.name === name ? [flatten!.(name, fn.args)...;] : [fn]
flatten!(name, x) = x

Base.sort!(fn::Fn) = (sort!(fn.args; lt = _sort_lt); fn)
# FIXME
const _order = [Fn, Variable, Constant]
function _sort_lt(f::Fn, g::Fn)
    f.name == g.name || return f.name < g.name
    length(f) == length(g) || return length(f) > length(g)
    all(p -> _sort_lt(p...), zip(f, g))
end
_sort_lt(a::Constant, b::Constant) = repr(get(a)) < repr(get(b))
_sort_lt(a::Variable, b::Variable) = a.name < b.name ? true : a.index < b.index
_sort_lt(::A, ::B) where {A<:Term,B<:Term} =
    findfirst((T -> A <: T), _order) < findfirst((T -> B <: T), _order)
