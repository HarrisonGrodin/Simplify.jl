struct StandardImages <: AbstractImages
    images::Dict{Any,AbstractSet}
    StandardImages(xs...) = new(Dict(xs...))
end
function image(ex::Expr, i::StandardImages)
    haskey(i.images, ex) && return i.images[ex]
    ex.head === :call || return TypeSet(Any)

    f, args = ex.args[1], ex.args[2:end]
    sig = f, length(args)

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

set_context!(Context(
    props = [
        Flat.([
            +,
            *,
            &,
            |,
        ]);
        Orderless.([
            +,
            &,
            |,
        ]);
    ],
    images = StandardImages(),
))
