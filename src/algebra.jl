set_context!(Context([
    Associative.([
        +,
        *,
        &,
        |,
    ]);
    Commutative.([
        +,
        &,
        |,
    ]);
    Closure(+, TypeSet(Number))
    Closure(+, TypeSet(Int))
    Closure(-, TypeSet(Number))
    Closure(-, TypeSet(Int))
    Closure(*, TypeSet(Number))
    Closure(*, TypeSet(Int))
    Signature(inv, [TypeSet(Number)], TypeSet(Number))
    Signature(/, [TypeSet(Number), TypeSet(Number)], TypeSet(Float64))
    Signature(^, [Positive, TypeSet(Number)], Positive)
    Signature(^, [Zero, TypeSet(Number)], Set([0, 1]))
    Signature(^, [TypeSet(Number), Even], Nonnegative)
    Signature(^, [TypeSet(Real), TypeSet(Real)], TypeSet(Real))
    Signature(^, [TypeSet(Number), TypeSet(Number)], TypeSet(Number))
    Signature(abs, [TypeSet(Number)], Nonnegative)
    Signature(sqrt, [TypeSet(Real)], Nonnegative)
    Signature(sin, [TypeSet(Real)], GreaterThan{Number}(-1, true) ∩ LessThan{Number}(1, true))
    Signature(cos, [TypeSet(Real)], GreaterThan{Number}(-1, true) ∩ LessThan{Number}(1, true))
    Signature(tan, [TypeSet(Number)], TypeSet(Number))
    Signature(log, [TypeSet(Number)], TypeSet(Float64))
    Signature(diff, [TypeSet(Number), TypeSet(Number)], TypeSet(Number))  # FIXME
    Closure(&, TypeSet(Bool))
    Closure(|, TypeSet(Bool))
    Signature(!, [TypeSet(Bool)], TypeSet(Bool))
]))
