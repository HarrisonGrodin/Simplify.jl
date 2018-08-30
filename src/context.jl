using SpecialSets

export with_context, set_context!, AlgebraContext


"""
    image(x, t::T) -> AbstractSet

Given image generator `t`, return the image of `x`.
"""
function image end

abstract type AbstractImages end
image(::Expr, ::AbstractImages) = TypeSet(Any)
image(x::Variable, ::AbstractImages) = x.image
image(x, ::AbstractImages) = Set([x])

struct EmptyImages <: AbstractImages end

struct StandardImages <: AbstractImages
    images::Dict{Term,AbstractSet}
    StandardImages(xs...) = new(Dict(xs...))
end
function image(ex::Expr, i::StandardImages)
    haskey(i.images, Term(ex)) && return i.images[ex]

    fn = property(Fn2, ex)
    fn === nothing && return TypeSet(Any)
    sig = fn.name, length(fn.args)

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


function property(::Type{Fn2}, ex::Expr)
    ex.head === :call || return
    Fn2(ex.args[1], ex.args[2:end])
end


function property(::Type{Flat}, ex::Expr)
    fn = property(Fn2, ex)
    fn === nothing && return
    length(fn.args) ≥ 2 || return

    haskey(CONTEXT.props, fn.name) || return
    Flat ∈ CONTEXT.props[fn.name]  || return

    args = copy(fn.args)
    i = 1
    while i ≤ length(args)
        x = property(Fn2, args[i])
        if x !== nothing && x.name == fn.name
            splice!(args, i, x.args)
        else
            i += 1
        end
    end

    Flat(fn.name, args)
end

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
