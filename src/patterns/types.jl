export Variable, Fn, TypeSet, Constant


struct Variable <: AbstractExpr
    name::Symbol
end
Base.match(x::Variable, ex::AbstractExpr) = Substitution(x => ex)
Base.replace(x::Variable, sub) = get(sub, x, x)


struct Fn{F,N} <: AbstractExpr
    args::SVector{N,AbstractExpr}
end
Fn{F,N}(xs::Vararg{<:AbstractExpr,N}) where {F,N} = Fn{F,N}(xs)
Fn{F}(xs::Vararg{Any,N}) where {F,N} = Fn{F,N}(xs)
function Base.match(p::Fn{F,N}, ex::Fn{F,N}) where {F,N}
    N == 0 && return Substitution()

    subs = match.(p.args, ex.args)
    any(x -> x === nothing, subs) && return nothing

    result = Substitution()
    for sub ∈ subs
        for (k, v) in sub
            haskey(result, k) && result[k] ≠ v && return nothing
            result[k] = v
        end
    end

    result
end
Base.replace(p::Fn{F,N}, sub) where {F,N} = Fn{F,N}(replace.(p.args, Ref(sub)))


struct TypeSet{T} <: AbstractExpr end
Base.match(::TypeSet{T}, ::TypeSet{<:T}) where {T} = Substitution()


struct Constant{T} <: AbstractExpr
    value::T
end
Base.get(x::Constant) = x.value
Base.match(a::Constant{T}, b::Constant{<:T}) where {T} =
    get(a) == get(b) ? Substitution() : nothing
Base.match(::TypeSet{T}, ::Constant{<:T}) where {T} = Substitution()
