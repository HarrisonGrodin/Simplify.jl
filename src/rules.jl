export rules


_strategy(::Val{S}) where {S} = string(S)
macro term(v::Val, ex)
    strategy = _strategy(v)
    :(throw(ArgumentError("Undefined @term strategy: " * $strategy)))
end
macro term(strategy::Symbol, expr)
    esc(:(@term $(Val(strategy)) $expr))
end

macro term(::Val{:RULES}, ex)
    args = map(ex.args) do pair
        p, a, b = pair.args
        @assert p == :(=>)
        a, b = Meta.quot(a), Meta.quot(b)
        :(PatternRule{Term}(convert(Term, $a), convert(Term, $b)))
    end
    :(TermRewritingSystem([$(args...)]))
end


rules(set::Symbol=:STANDARD, args...; kwargs...) = rules(Val(set), args...; kwargs...)


rules(::Val{:STANDARD}) = [
    @term RULES [
        x + 0      => x
        0 + x      => x
        x * 1      => x
        1 * x      => x
        x * 0      => 0
        0 * x      => 0
        x + -y     => x - y
        0 - x      => -x
        x - x      => 0
        x * inv(y) => x / y
        x / 1      => x
        -x / y     => -(x / y)
        x / -y     => -(x / y)
        x ^ 0      => one(x)
        # x^(a + b)  => x^a * x^b  # FIXME
    ]
    TRS(
        EvalRule(+),
        EvalRule(-),
        EvalRule(*),
        EvalRule(zero),
        EvalRule(one),
    )
    rules(:ABSOLUTE_VALUE)
    rules(:BOOLEAN)
    rules(:LOGARITHM)
    rules(:TRIGONOMETRY)
]


function rules(::Val{:ABSOLUTE_VALUE})
    nn, neg = Variable.([:nn, :neg], [Nonnegative, Negative])
    [
        @term RULES [
            abs($nn)   => $nn
            abs($neg)  => -$neg
            abs(-a)    => abs(a)
            abs(a * b) => abs(a) * abs(b)
            abs(a / b) => abs(a) / abs(b)
        ]
        TRS(
            EvalRule(abs),
        )
    ]
end


rules(::Val{:BOOLEAN}; and=:&, or=:|, neg=:!) = [
    @term RULES [
        $or(x, false) => x
        $and(x, true) => x

        $or(x, true) => true
        $and(x, false) => false

        $or(x, x) => x
        $and(x, x) => x

        $or(x, $and(x, y)) => x
        $and(x, $or(x, y)) => x

        $or(x, $neg(x)) => true
        $and(x, $neg(x)) => false

        $neg($neg(x)) => x
    ];
    TRS(
        EvalRule(and, &),
        EvalRule(or,  |),
        EvalRule(neg, !),
    );
]

#=
FIXME Notation
rules(::Val{:LAPLACE}) = @term RULES [
    laplace(1) => 1/s #1
    laplace(e^(a*t)) => 1/(s-a) #2
    laplace(t^n) where n isa Int => factorial(n) / s^(n+1)
]
=#

rules(::Val{:LOGARITHM}) = @term RULES [
    log(b, b) => 1
    log(b, 1) => 0

    log(b, b ^ x) => x
    b ^ log(b, x) => x

    log(b, x ^ r) => r * log(b, x)

    log(b, x * y) => log(b, x) + log(b, y)
    log(b, x / y) => log(b, x) - log(b, y)
]

rules(::Val{:TRIGONOMETRY}) = @term RULES [
    # Common angles
    sin(0) => 0
    cos(0) => 1
    tan(0) => 0

    sin(π / 6) => 1 / 2
    cos(π / 6) => √3 / 2
    tan(π / 6) => √3 / 3

    sin(π / 4) => √2 / 2
    cos(π / 4) => √2 / 2
    tan(π / 4) => 1

    sin(π / 3) => √3 / 2
    cos(π / 3) => 1 / 2
    tan(π / 3) => √3

    sin(π / 2) => 1
    cos(π / 2) => 0
    # tan(π / 2) => # TODO: infinite/undefined


    # Definitions of relations
    sin(θ) / cos(θ) => tan(θ)
    cos(θ) / sin(θ) => cot(θ)
    1 / cos(θ) => sec(θ)
    1 / sec(θ) => cos(θ)
    1 / sin(θ) => csc(θ)
    1 / csc(θ) => sin(θ)
    1 / tan(θ) => cot(θ)
    1 / cot(θ) => tan(θ)

    # Pythagorean identities
    sin(θ)^2 + cos(θ)^2 => one(θ)
    one(θ) + tan(θ)^2 => sec(θ)^2  # NOTE: will not match any one constants
    one(θ) + cot(θ^2) => csc(θ)^2

    # Negative angles
    sin(-θ) => -sin(θ)
    cos(-θ) => cos(θ)
    tan(-θ) => -tan(θ)
    csc(-θ) => -csc(θ)
    sec(-θ) => sec(θ)
    cot(-θ) => -cot(θ)

    # Periodic formulae
    #=
    FIXME where clause requires predicates
    sin(θ + 2πn) where n isa Int => sin(θ)
    cos(θ + 2πn) where n isa Int => cos(θ)
    tan(θ + πn) where n isa Int => tan(θ)
    csc(θ + 2πn) where n isa Int => csc(θ)
    sec(θ + 2πn) where n isa Int => sec(θ)
    cot(θ + πn) where n isa Int => cot(θ)
    =#

    # Double-angle formulae
    2sin(θ)cos(θ) => sin(2θ)
    cos(θ)^2 - sin(θ)^2 => cos(2θ)
    2cos(θ)^2 - 1 => cos(2θ)

    # Sum and difference formulae
    sin(α)cos(β) + cos(α)sin(β) => sin(α + β)
    sin(α)cos(β) - cos(α)sin(β) => sin(α - β)
    cos(α)cos(β) - sin(α)sin(β) => cos(α + β)
    cos(α)cos(β) + sin(α)sin(β) => cos(α - β)
    (tan(α) + tan(β)) / (1 - tan(α)tan(β)) => tan(α + β)
    (tan(α) - tan(β)) / (1 + tan(α)tan(β)) => tan(α - β)

    # Product to sum formulae
    cos(α - β) - cos(α + β) => 2sin(α)sin(β)
    cos(α - β) + cos(α + β) => 2cos(α)cos(β)
    sin(α + β) + sin(α - β) => 2sin(α)cos(β)
    sin(α + β) - sin(α - β) => 2cos(α)sin(β)

    # Sum to product formulae
    2sin((α + β) / 2)cos(α - β / 2) => sin(α) + sin(β)
    2cos((α + β) / 2)sin(α - β / 2) => sin(α) - sin(β)
    2cos((α + β) / 2)cos(α - β / 2) => cos(α) + cos(β)
    -2sin((α + β) / 2)sin(α - β / 2) => cos(α) - cos(β)

    # Cofunction formulae
    sin(π/2-θ) => cos(θ)
    cos(π/2-θ) => sin(θ)
    csc(π/2-θ) => sec(θ)
    sec(π/2-θ) => csc(θ)
    tan(π/2-θ) => cot(θ)
    cot(π/2-θ) => tan(θ)
]
