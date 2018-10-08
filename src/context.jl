using SpecialSets

export with_context, set_context!, Context


"""
    image(x, t::T) -> AbstractSet

Given image generator `t`, return the image of `x`.
"""
function image end
image(x) = image(x, CONTEXT)

abstract type AbstractImages end
struct EmptyImages <: AbstractImages end
image(::Expr     , ::AbstractImages) = TypeSet(Any)
image(x::Symbolic, ::AbstractImages) = x.image
image(::Variable , ::AbstractImages) = TypeSet(Any)
image(x          , ::AbstractImages) = Set([x])


struct Context
    props::Vector{Property}
    images::AbstractImages
    Context(; props=[], images=EmptyImages()) = new(props, images)
end

Base.broadcastable(ctx::Context) = Ref(ctx)
set_context!(context::Context) = (global CONTEXT = context)
function with_context(f, context::Context)
    global CONTEXT
    old = CONTEXT
    CONTEXT = context
    try
        f()
    finally
        CONTEXT = old
    end
end

image(x, ctx::Context) = image(x, ctx.images)

isvalid(::Standard) = true
isvalid(prop::P) where {P<:Union{Flat, Orderless}} = prop ∈ CONTEXT.props
isvalid(i::Image) = image(i.ex) ⊆ i.set
