import Base: isvalid

export with_context, get_context, set_context!, Context
export isvalid


abstract type Property end


struct Context
    props::Vector{Property}
end
Context(props::Property...) = Context(collect(props))
Base.vcat(ctx::Context, xs...) = Context(vcat(_props(ctx), _props.(xs)...))
Base.vcat(ctx::Context, ctxs::Context...) = Context(vcat(_props(ctx), _props.(ctxs)...))
_props(ctx::Context) = ctx.props
_props(p) = p


const CONTEXT = Ref(Context())

get_context() = CONTEXT[]
set_context!(context::Context) = (CONTEXT[] = context)
function with_context(f, context::Context)
    old = get_context()
    set_context!(context)
    try
        f()
    finally
        set_context!(old)
    end
end


"""
    isvalid(p::Property, ctx::Context) -> Bool

Returns the value corresponding to whether or not the given property object `p` is valid,
given context `ctx`.
"""
isvalid(q::Property, ctx::Context) = any(p -> implies(p, q, ctx), ctx.props)
isvalid(prop::Property) = isvalid(prop, get_context())
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
