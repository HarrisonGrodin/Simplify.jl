export Variable, Constant, Fn, Associative, Commutative

using StaticArrays


struct Variable <: Term
    name::Symbol
    index::UInt
end
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
SUBSCRIPTS = Dict{Int,Char}(
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
subscript(x::Integer) = map(reverse(digits(x))) do c
    SUBSCRIPTS[c]
end |> join
Base.string(x::Variable) = x.index == 0 ? string(x.name) : string(x.name, subscript(x.index))
Base.parse(x::Variable) = Symbol(string(x))


struct Constant{T} <: Term
    value::T
end
Base.get(x::Constant) = x.value
Base.parse(x::Constant) = get(x)


struct Fn{F,N} <: Term
    args::SVector{N,Term}
end
Fn{F}(args) where {F} = Fn{F,length(args)}(args)
Fn{F}(args::Vararg{Term,N}) where {F,N} = Fn{F,N}(args)
Fn{F,N}(args::Vararg{Term,N}) where {F,N} = Fn{F,N}(args)
Base.iterate(f::Fn) = iterate(f.args)
Base.iterate(f::Fn, start) = iterate(f.args, start)
Base.length(f::Fn{F,N}) where {F,N} = N
Base.getindex(f::Fn, key) = f.args[key]
Base.setindex(f::Fn{F,N}, val, key) where {F,N} = Fn{F,N}(setindex(f.args, val, key))
Base.map(f, fn::Fn{F,N}) where {F,N} = Fn{F,N}(map(f, fn.args))
function Base.parse(::Type{Fn}, props, ex::Expr) where {F}
    ex.head === :call || throw(ArgumentError("$(repr(ex)) is not a function call"))
    parse(Fn{ex.args[1]}, props, ex)
end
function Base.parse(::Type{Fn{F}}, props, ex::Expr) where {F}
    ex.head === :call || return parse(Term, props, ex)
    ex.args[1] === F || return parse(Term, props, ex)
    args = ex.args[2:end]
    Fn{F}(parse.(Term, props, args)...)
end
Base.parse(f::Fn{F}) where {F} = :($F($(parse.(f.args)...)))


struct Associative{F} <: Term
    args::Vector{Term}
    Associative{F}(args) where {F} = new{F}(flatten(Associative{F}, args))
end
Associative{F}(args::Term...) where {F} = Associative{F}(collect(args))
Base.:(==)(f::F, g::F) where {F<:Associative} = f.args == g.args
Base.:(==)(::Associative, ::Associative) = false
Base.iterate(f::Associative) = iterate(f.args)
Base.iterate(f::Associative, state) = iterate(f.args, state)
Base.length(f::Associative) = length(f.args)
Base.getindex(f::Associative, inds...) = getindex(f.args, inds...)
Base.setindex(f::Associative{F}, val, key) where {F} = Associative{F}(setindex!(copy(f.args), val, key))
Base.parse(f::Associative{F}) where {F} = :($F($(parse.(f.args)...)))
function Base.parse(::Type{Associative}, props, ex::Expr) where {F}
    ex.head === :call || throw(ArgumentError("$(repr(ex)) is not a function call"))
    parse(Associative{ex.args[1]}, props, ex)
end
function Base.parse(::Type{Associative{F}}, props, ex::Expr) where {F}
    ex.head === :call || return parse(Term, ex)
    ex.args[1] === F || return parse(Term, ex)
    args = ex.args[2:end]
    Associative{F}(parse.(Associative{F}, props, args)...)
end
Base.parse(::Type{<:Associative}, props, x) = parse(Term, props, x)

flatten(::Type{F}, args) where {F} = [_flatten.(F, args)...;]
_flatten(::Type{F}, f::F) where {F} = _flatten(F, f.args)
_flatten(::Type, x) = x


struct Commutative{T<:Term} <: Term
    fn::T
end
Base.:(==)(f::F, g::F) where {F<:Commutative} = f.fn == g.fn
Base.:(==)(::Commutative, ::Commutative) = false
Base.iterate(f::Commutative) = iterate(f.fn)
Base.iterate(f::Commutative, state) = iterate(f.fn, state)
Base.length(f::Commutative) = length(f.fn)
Base.getindex(f::Commutative, inds...) = getindex(f.fn, inds...)
Base.setindex(f::Commutative{T}, val, key) where {T} = Commutative{T}(setindex(f.fn, val, key))
Base.parse(f::Commutative) = parse(f.fn)
function Base.parse(::Type{Commutative}, props, ex::Expr) where {F}
    ex.head === :call || throw(ArgumentError("$(repr(ex)) is not a function call"))
    parse(Commutative{Fn{ex.args[1]}}, props, ex)
end
Base.parse(::Type{Commutative{T}}, props, ex) where {T} = Commutative(parse(T, props, ex))
