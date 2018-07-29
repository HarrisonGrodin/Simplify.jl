"""
    image(x, t::T) -> AbstractSet

Given image generator `t`, return the image of `x`.
"""
function image end

abstract type AbstractImages end

struct StandardImages <: AbstractImages end
function (i::StandardImages)(fn::Fn)
    sig = fn_name(fn), length(fn)
    sig == (:^, 2) && i(fn[1]) ⊆ Positive && return Positive
    sig == (:^, 2) && i(fn[1]) ⊆ Zero && return Set([0, 1])
    sig == (:^, 2) && i(fn[2]) ⊆ Even && return Nonnegative
    sig == (:abs, 1) && return Nonnegative
    sig == (:sqrt, 1) && return Nonnegative
    sig == (:sin, 1) && return GreaterThan(-1, true) ∩ LessThan(1, true)
    sig == (:cos, 1) && return GreaterThan(-1, true) ∩ LessThan(1, true)
    TypeSet(Number)
end
(::StandardImages)(x::Constant) = Set([get(x)])
(::StandardImages)(x::Variable) = x.image
(::StandardImages)(x) = TypeSet(Number)
image(x, i::StandardImages) = i(x)


abstract type AbstractContext end
Base.broadcastable(ctx::AbstractContext) = Ref(ctx)

struct AlgebraContext <: AbstractContext
    props::Dict{Symbol,Type}
    consts::Dict{Symbol,Any}
    images::AbstractImages
end
AlgebraContext(props, consts) = AlgebraContext(props, consts, StandardImages())


function Base.convert(::Type{Term}, ex::Expr, ctx::AlgebraContext)
    ex.head === :call || throw(ArgumentError("Invalid expression head: $(repr(ex.head))"))
    typ = get(ctx.props, ex.args[1], Fn)
    convert(typ, ex, ctx)
end
Base.convert(::Type{Term}, x::Symbol, ctx::AlgebraContext) =
    haskey(ctx.consts, x) ? convert(Constant, ctx.consts[x]) : convert(Variable, x)
Base.convert(::Type{Term}, t::Term, ::AlgebraContext) = t
Base.convert(::Type{Term}, x, ::AlgebraContext) = convert(Constant, x)

image(x, ctx::AlgebraContext) = image(x, ctx.images)


const DEFAULT_CONTEXT = AlgebraContext(
    Dict(
        :+  => Commutative{Associative},
        :++ => Associative,
        :*  => Associative,
    ),
    Dict(
        :π  => π,
    ),
    StandardImages(),
)
Base.convert(::Type{T}, ex) where {T<:Term} = convert(T, ex, DEFAULT_CONTEXT)
image(x) = image(x, DEFAULT_CONTEXT)
