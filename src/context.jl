using SpecialSets

export with_context, set_context!, AlgebraContext


"""
    image(x, t::T) -> AbstractSet

Given image generator `t`, return the image of `x`.
"""
function image end

abstract type AbstractImages end
image(::Union{Expr, Symbolic}, ::AbstractImages) = TypeSet(Any)
image(x::Variable, ::AbstractImages) = x.image
image(x, ::AbstractImages) = Set([x])

struct EmptyImages <: AbstractImages end

struct StandardImages <: AbstractImages
    images::Dict{Any,AbstractSet}
    StandardImages(xs...) = new(Dict(xs...))
end
function image(ex::Expr, i::StandardImages)
    haskey(i.images, ex) && return i.images[ex]
    ex.head === :call || return TypeSet(Any)

    name, args = ex.args[1], ex.args[2:end]
    sig = name, length(args)

    sig == (/, 2) && return TypeSet(Float64)
    sig == (^, 2) && image(args[1], i) ⊆ Positive && return Positive
    sig == (^, 2) && image(args[1], i) ⊆ Zero && return Set([0, 1])
    sig == (^, 2) && image(args[2], i) ⊆ Even && return Nonnegative
    sig == (abs, 1) && return Nonnegative
    sig == (sqrt, 1) && return Nonnegative
    sig == (sin, 1) && return GreaterThan{Number}(-1, true) ∩ LessThan{Number}(1, true)
    sig == (cos, 1) && return GreaterThan{Number}(-1, true) ∩ LessThan{Number}(1, true)
    sig == (log, 1) && return TypeSet(Float64)

    BOOL = TypeSet(Bool)
    sig == (&, 2) && image(args[1], i) ⊆ BOOL && image(args[2], i) ⊆ BOOL && return BOOL
    sig == (|, 2) && image(args[1], i) ⊆ BOOL && image(args[2], i) ⊆ BOOL && return BOOL
    sig == (!, 1) && image(args[1], i) ⊆ BOOL && return BOOL

    TypeSet(Number)
end
image(x::Symbolic, i::StandardImages) = haskey(i.images, x) ? i.images[x] : TypeSet(Any)


abstract type AbstractContext end
Base.broadcastable(ctx::AbstractContext) = Ref(ctx)

struct AlgebraContext <: AbstractContext
    props::Dict{Any,Vector{Type{<:Property}}}
    images::AbstractImages
    AlgebraContext(; props=Dict(), images=EmptyImages()) = new(props, images)
end


image(x, ctx::AlgebraContext) = image(x, ctx.images)


const DEFAULT_CONTEXT = AlgebraContext(
    props = Dict(
        Symbolic(:+)  => [Flat, Orderless],
        Symbolic(:++) => [Flat],
        Symbolic(:*)  => [Flat],
        (+)           => [Flat, Orderless],
        (*)           => [Flat],
        (&)           => [Flat, Orderless],
        (|)           => [Flat, Orderless],
    ),
    images = StandardImages(),
)


CONTEXT = DEFAULT_CONTEXT
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

property(::Type{Standard}, ::Any) = Standard
function property(::Type{Flat}, ex::Expr)
    ex.head === :call   || return
    length(ex.args) ≥ 2 || return

    name = ex.args[1]
    haskey(CONTEXT.props, name) || return
    Flat ∈ CONTEXT.props[name]  || return

    Flat
end

function property(::Type{Orderless}, ex::Expr)
    ex.head === :call   || return
    length(ex.args) ≥ 2 || return

    name = ex.args[1]
    haskey(CONTEXT.props, name)     || return
    Orderless ∈ CONTEXT.props[name] || return

    Orderless
end
