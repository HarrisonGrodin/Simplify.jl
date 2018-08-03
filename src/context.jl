using SpecialSets

export with_context, set_context!, AlgebraContext


"""
    image(x, t::T) -> AbstractSet

Given image generator `t`, return the image of `x`.
"""
function image end

abstract type AbstractImages end
image(x::Variable, ::AbstractImages) = x.image
image(x::Constant, ::AbstractImages) = Set([get(x)])

struct StandardImages <: AbstractImages end
function image(fn::Fn, i::StandardImages)
    sig = fn.name, length(fn)
    sig == (:^, 2) && image(fn[1], i) ⊆ Positive && return Positive
    sig == (:^, 2) && image(fn[1], i) ⊆ Zero && return Set([0, 1])
    sig == (:^, 2) && image(fn[2], i) ⊆ Even && return Nonnegative
    sig == (:abs, 1) && return Nonnegative
    sig == (:sqrt, 1) && return Nonnegative
    sig == (:sin, 1) && return GreaterThan{Number}(-1, true) ∩ LessThan{Number}(1, true)
    sig == (:cos, 1) && return GreaterThan{Number}(-1, true) ∩ LessThan{Number}(1, true)
    TypeSet(Number)
end


abstract type AbstractContext end
Base.broadcastable(ctx::AbstractContext) = Ref(ctx)

struct AlgebraContext <: AbstractContext
    props::Dict{Symbol,Vector{Type{<:Property}}}
    consts::Dict{Symbol,Any}
    images::AbstractImages
    AlgebraContext(; props=Dict(), consts=Dict(), images=StandardImages()) =
        new(props, consts, images)
end


Base.convert(::Type{Term}, ex::Expr, ctx::AlgebraContext) = convert(Fn, ex)
Base.convert(::Type{Term}, x::Symbol, ctx::AlgebraContext) =
    haskey(ctx.consts, x) ? convert(Constant, ctx.consts[x]) : convert(Variable, x)
Base.convert(::Type{Term}, t::Term, ::AlgebraContext) = t
Base.convert(::Type{Term}, x, ::AlgebraContext) = convert(Constant, x)

image(x, ctx::AlgebraContext) = image(x, ctx.images)


const DEFAULT_CONTEXT = AlgebraContext(
    props = Dict(
        :+  => [Flat, Orderless],
        :++ => [Flat],
        :*  => [Flat],
    ),
    consts = Dict(
        :π  => π,
    ),
    images = StandardImages(),
)


CONTEXT = DEFAULT_CONTEXT
Base.convert(::Type{Term}, ex) = convert(Term, ex, CONTEXT)
image(x) = image(x, CONTEXT)

set_context!(context::AbstractContext) = (global CONTEXT = context)

function with_context(f, context::AbstractContext)
    global CONTEXT
    old = CONTEXT
    CONTEXT = context
    try
        f()
    finally
        CONTEXT = old
    end
end


function property(::Type{Flat}, fn::Fn)
    length(fn) ≥ 2 || return nothing
    haskey(CONTEXT.props, fn.name) || return nothing
    Flat ∈ CONTEXT.props[fn.name] || return nothing
    Flat()
end
function property(::Type{Orderless}, fn::Fn)
    length(fn) ≥ 2 || return nothing
    haskey(CONTEXT.props, fn.name) || return nothing
    Orderless ∈ CONTEXT.props[fn.name] || return nothing
    Orderless()
end
