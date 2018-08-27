using SpecialSets

export with_context, set_context!, AlgebraContext


"""
    image(x, t::T) -> AbstractSet

Given image generator `t`, return the image of `x`.
"""
function image end

abstract type AbstractImages end
image(t::Term, ::AbstractImages) = TypeSet(Any)  # FIXME

struct EmptyImages <: AbstractImages end

struct StandardImages <: AbstractImages
    images::Dict{Term,AbstractSet}
    StandardImages(xs...) = new(Dict(xs...))
end
function image(t::Term, i::StandardImages)
    haskey(i.images, t) && return i.images[t]

    ex = get(t)
    is_constant(t) && return Set([ex])
    ex isa Expr && ex.head === :call || return TypeSet(Any)
    sig = get(t[1]), length(t) - 1

    sig == (/, 2) && return TypeSet(Float64)
    sig == (^, 2) && image(fn[1], i) ⊆ Positive && return Positive
    sig == (^, 2) && image(fn[1], i) ⊆ Zero && return Set([0, 1])
    sig == (^, 2) && image(fn[2], i) ⊆ Even && return Nonnegative
    sig == (abs, 1) && return Nonnegative
    sig == (sqrt, 1) && return Nonnegative
    sig == (sin, 1) && return GreaterThan{Number}(-1, true) ∩ LessThan{Number}(1, true)
    sig == (cos, 1) && return GreaterThan{Number}(-1, true) ∩ LessThan{Number}(1, true)
    sig == (log, 1) && return TypeSet(Float64)

    BOOL = TypeSet(Bool)
    sig == (&, 2) && image(fn[1], i) ⊆ BOOL && image(fn[2], i) ⊆ BOOL && return BOOL
    sig == (|, 2) && image(fn[1], i) ⊆ BOOL && image(fn[2], i) ⊆ BOOL && return BOOL
    sig == (!, 1) && image(fn[1], i) ⊆ BOOL && return BOOL

    TypeSet(Number)
end


abstract type AbstractContext end
Base.broadcastable(ctx::AbstractContext) = Ref(ctx)

struct AlgebraContext <: AbstractContext
    props::Dict{Any,Vector{Type{<:Property}}}
    images::AbstractImages
    orderless::Dict{Symbol,AbstractSet}
    AlgebraContext(; props=Dict(), images=EmptyImages(), orderless=Dict()) =
        new(props, images, orderless)
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
    orderless = Dict(
        :* => TypeSet(Number),
    ),
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


function property(::Type{Flat}, fn::Fn)
    fn.ex.head === :call || return
    length(fn.ex.args) ≥ 3 || return  # includes function head

    name = get(fn[1])

    haskey(CONTEXT.props, name) || return
    Flat ∈ CONTEXT.props[name] || return

    args = copy(fn[2:end])
    i = 1
    while i ≤ length(args)
        x = args[i]
        if x isa Fn && get(x[1]) == name
            splice!(args, i, x.ex.args[2:end])
        else
            i += 1
        end
    end

    Flat(name, args)
end
function flatten!(fn::Fn)
    flat = flatten!(fn.name, fn)
    append!(empty!(fn.args), flat)
    fn
end
flatten!(name, fn::Fn) = fn.name === name ? [flatten!.(name, fn.args)...;] : [fn]
flatten!(name, x) = x

# function property(::Type{Orderless}, fn::Fn)
#     length(fn) ≥ 2 || return nothing
#     Orderless ∈ get(CONTEXT.props, fn.name, []) && return Orderless(fn.name, collect(fn), [])
#
#     if haskey(CONTEXT.orderless, fn.name)
#         args = collect(fn)
#         inds = image.(args) .⊆ Ref(CONTEXT.orderless[fn.name])
#         any(inds) || return nothing
#         return Orderless(fn.name, args[inds], args[(!).(inds)])
#     end
#
#     nothing
# end
