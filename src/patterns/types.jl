export Variable, Fn, TypeSet, Constant

using StaticArrays


struct Variable <: Term
    name::Symbol
    index::UInt
end
Variable(name::Symbol) = Variable(name, 0)
SUBSCRIPTS = Dict{Char,Char}(
    '0' => '₀',
    '1' => '₁',
    '2' => '₂',
    '3' => '₃',
    '4' => '₄',
    '5' => '₅',
    '6' => '₆',
    '7' => '₇',
    '8' => '₈',
    '9' => '₉',
)
subscript(c::Char) = get(SUBSCRIPTS, c, c)
subscript(s::String) = map(subscript, s)
subscript(x::Integer) = join(map(subscript ∘ string, reverse(digits(x))))
Base.string(x::Variable) = string(x.name, subscript(x.index))
Base.parse(x::Variable) = Symbol(string(x))


struct Fn{F,N} <: Term
    args::SVector{N,Term}
end
Fn{F}(args) where {F} = Fn{F,length(args)}(args)
Fn{F}(args::Vararg{Term,N}) where {F,N} = Fn{F,N}(args)
Fn{F,N}(args::Vararg{Term,N}) where {F,N} = Fn{F,N}(args)
Base.iterate(f::Fn) = iterate(f.args)
Base.iterate(f::Fn, start) = iterate(f.args, start)
Base.length(f::Fn{F,N}) where {F,N} = N
Base.copy(f::Fn{F,N}) where {F,N} = Fn{F,N}(copy(f.args))
Base.getindex(f::Fn, key) = f.args[key]
Base.setindex(f::Fn{F,N}, val, key) where {F,N} = Fn{F,N}(setindex(f.args, val, key))
Base.map(f, fn::Fn{F,N}) where {F,N} = Fn{F,N}(map(f, fn.args))
Base.parse(f::Fn{F}) where {F} = :($F($(parse.(f.args)...)))


struct TypeSet{T} <: Term end
Base.match(::TypeSet{T}, ::TypeSet{<:T}) where {T} = Substitution()
Base.parse(::TypeSet{T}) where {T} = :(::$T)


struct Constant{T} <: Term
    value::T
end
Base.get(x::Constant) = x.value
Base.match(a::Constant{T}, b::Constant{<:T}) where {T} =
    get(a) == get(b) ? Substitution() : nothing
Base.match(::TypeSet{T}, ::Constant{<:T}) where {T} = Substitution()
Base.parse(x::Constant) = get(x)
