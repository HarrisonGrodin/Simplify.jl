export Variable, Fn, TypeSet, Constant


struct Variable <: Term
    name::Symbol
    index::UInt
end
Variable(name::Symbol) = Variable(name, 0)


struct Fn <: Term
    head::Symbol
    args::Vector{Term}
end
Fn(head, args::Term...) = Fn(Symbol(head), collect(args))
Base.iterate(f::Fn) = iterate(f.args)
Base.iterate(f::Fn, start) = iterate(f.args, start)
Base.length(f::Fn) = length(f.args)
Base.getindex(f::Fn, key) = f.args[key]
Base.setindex(f::Fn, val, key) = Fn(f.head, [f.args[1:key-1]; val; f.args[key+1:end]])
Base.map(f, fn::Fn) = Fn(fn.head, map(f, fn.args))
Base.:(==)(f::Fn, g::Fn) = f.head == g.head && f.args == g.args


struct TypeSet{T} <: Term end
Base.match(::TypeSet{T}, ::TypeSet{<:T}) where {T} = Substitution()


struct Constant{T} <: Term
    value::T
end
Base.get(x::Constant) = x.value
Base.match(a::Constant{T}, b::Constant{<:T}) where {T} =
    get(a) == get(b) ? Substitution() : nothing
Base.match(::TypeSet{T}, ::Constant{<:T}) where {T} = Substitution()
