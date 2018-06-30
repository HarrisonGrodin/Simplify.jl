export rules

rules(set::Symbol=:STANDARD, args...; kwargs...) = rules(Val(set), args...; kwargs...)


rules(::Val{:STANDARD}) = [(@term PAIRS [
    x + 0      => x,
    0 + x      => x,
    x * 1      => x,
    1 * x      => x,
    x * 0      => 0,
    0 * x      => 0,
    x + -y     => x - y,
    x - x      => 0,
    x * inv(y) => x / y,
]); rules(:BOOLEAN)]


rules(::Val{:BOOLEAN}; and=:&, or=:|, neg=:!) = @term PAIRS [
    $or(x, false) => x,
    $and(x, true) => x,

    $or(x, true) => true,
    $and(x, false) => false,

    $or(x, x) => x,
    $and(x, x) => x,

    $or(x, $and(x, y)) => x,
    $and(x, $or(x, y)) => x,

    $or(x, $neg(x)) => true,
    $and(x, $neg(x)) => false,

    $neg($neg(x)) => x,
]
