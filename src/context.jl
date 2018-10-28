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


"""
    isvalid(p::Property, ctx::Context) -> Bool

Returns the value corresponding to whether or not the given property object `p` is valid,
given context `ctx`.
"""
isvalid(q::Property, ctx::Context) = any(p -> implies(p, q, ctx), ctx.props)
isvalid(prop::Property) = isvalid(prop, CONTEXT)
isvalid(ctx::Context) = Base.Fix2(isvalid, ctx)


"""
    implies(p::Property, q::Property, , ctx::Context) -> Bool

Determine whether property `p` implies property `q` given context `ctx`.

!!! note
    Since `p` implies `q`, it is implied that `q` is more basic of a property than `p`.
"""
implies(p::Property, q::Property, ctx::Context)

implies(::Property, ::Property, ::Context) = false
implies(p::P, q::P, ::Context) where {P<:Property} = p == q
