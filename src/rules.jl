export rules

rules(set::Symbol=:STANDARD, args...; kwargs...) = rules(Val(set), args...; kwargs...)


rules(::Val{:STANDARD}) = @term PAIRS [
    x + 0 => x,
    0 + x => x,
]


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
