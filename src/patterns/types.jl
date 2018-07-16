export Variable, Constant, Fn

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
Base.parse(f::Fn{F}) where {F} = :($F($(parse.(f.args)...)))
