using SpecialSets

export rules


_strategy(::Val{S}) where {S} = string(S)
macro term(v::Val, ex)
    strategy = _strategy(v)
    :(throw(ArgumentError("Undefined @term strategy: " * $strategy)))
end
macro term(strategy::Symbol, expr)
    esc(:(@term $(Val(strategy)) $expr))
end

macro term(::Val{:AXIOMS}, ex)
    args = map(ex.args) do axiom
        @assert axiom.head == :tuple
        a, b = axiom.args
        a, b = Meta.quot(a), Meta.quot(b)
        :(convert.(Term, ($a, $b)))
    end
    Expr(ex.head, args...)
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


rules(set::Symbol, args...; kwargs...) = rules(Val(set), args...; kwargs...)


rules() = [
    rules(:BASIC)
    rules(:ABSOLUTE_VALUE)
    rules(:BOOLEAN)
    rules(:CALCULUS)
    rules(:LOGARITHM)
    rules(:TRIGONOMETRY)
    rules(:TYPES)
]

function rules(::Val{:BASIC})
    a, b = Variable.([:a, :b], Ref(Nonzero))

    [
        @term RULES [
            x - y  => x + -y
            x / $a => x * inv($a)

            x + -x       => zero(x)
            $a * inv($a) => one($a)

            -(x + y)   => -y + -x
            inv($a * $b) => inv($b) * inv($a)

            -(-x)      => x
            inv(inv($a)) => $a

            -1 * x     => -x
            -x * y     => -(x * y)
            inv(-$a)     => -inv($a)

            x ^ 0      => one(x)
            x ^ 1      => x
            x^a * x^b  => x^(a + b)
        ]
        TRS(
            EvalRule(+),
            EvalRule(-),
            EvalRule(*),
        )
    ]
end


function rules(::Val{:ABSOLUTE_VALUE})
    nn, neg = Variable.([:nn, :neg], [Nonnegative, Negative])
    [
        @term RULES [
            abs($nn)   => $nn
            abs($neg)  => -$neg
            abs(-a)    => abs(a)
            abs(a * b) => abs(a) * abs(b)
            abs(inv(a)) => inv(abs(a))
        ]
        TRS(
            EvalRule(abs),
        )
    ]
end


function rules(::Val{:BOOLEAN}; and=:&, or=:|, neg=:!)
    x, y = Variable.([:x, :y], Ref(TypeSet(Bool)))

    [
        @term RULES [
            $or($x, false) => $x
            $and($x, true) => $x

            $or($x, true)   => true
            $and($x, false) => false

            $or($x, $x)  => $x
            $and($x, $x) => $x

            $or($x, $and($x, $y)) => $x
            $and($x, $or($x, $y)) => $x

            $or($x, $neg($x))  => true
            $and($x, $neg($x)) => false

            $neg($neg($x)) => $x
        ];
        TRS(
            EvalRule(and, &),
            EvalRule(or,  |),
            EvalRule(neg, !),
        );
    ]
end


function diff(M, fn, arity)
    M === :Base || return
    args = Symbol.(:_, 1:arity)
    f = Expr(:call, fn, args...)

    partials = DiffRules.diffrule(M, fn, args...)

    if arity == 1
        rhs = _diff(partials, args[1])
    else
        rhs = Expr(:call, :+, _diff.(partials, args)...)
    end

    lhs = :(diff($f, x))
    convert(Term, lhs) => convert(Term, rhs)
end
_diff(p, a, x=:x) = :($p * diff($a, $x))
function rules(::Val{:CALCULUS})
    rules = []
    for (M, fn, arity) ∈ DiffRules.diffrules()
        try
            rule = diff(M, fn, arity)
            rule === nothing && continue
            push!(rules, rule)
        catch
        end
    end

    TRS(
        rules...,
        @term(diff(x, x)) => @term(one(x)),
        DiffRule(),
    )
end


#=
FIXME Notation
rules(::Val{:LAPLACE}) = @term RULES [
    laplace(1) => 1/s #1
    laplace(e^(a*t)) => 1/(s-a) #2
    laplace(t^n) where n isa Int => factorial(n) / s^(n+1)
]
=#


function rules(::Val{:LOGARITHM})
    m = Variable(:m, NotEqual(1))
    n = Variable(:n, NotEqual(0, 1))

    @term RULES [
        log($n, $n) => 1
        log($m, 1) => 0

        log($n, $n ^ x) => x
        b ^ log(b, x) => x

        log(b, x ^ r) => r * log(b, x)

        log(b, x * y) => log(b, x) + log(b, y)
        log(b, inv(x)) => -log(b, x)

        log(a, b) * log(b, c) => log(a, c)
    ]
end

function rules(::Val{:TRIGONOMETRY})
    x = Variable(:x, Nonzero)

    @term RULES [
        # Common angles
        sin(0) => 0
        cos(0) => 1
        tan(0) => 0

        sin(π * inv(6)) => 1 / 2
        cos(π * inv(6)) => √3 / 2
        tan(π * inv(6)) => √3 / 3

        sin(π * inv(4)) => √2 / 2
        cos(π * inv(4)) => √2 / 2
        tan(π * inv(4)) => 1

        sin(π * inv(3)) => √3 / 2
        cos(π * inv(3)) => 1 / 2
        tan(π * inv(3)) => √3

        sin(π * inv(2)) => 1
        cos(π * inv(2)) => 0
        # tan(π * inv(2)) => # TODO: infinite/undefined


        # Definitions of relations
        sin(θ) * sec(θ) => tan(θ)
        cos(θ) * csc(θ) => cot(θ)
        inv(cos(θ)) => sec(θ)
        inv(sec(θ)) => cos(θ)
        inv(sin(θ)) => csc(θ)
        inv(csc(θ)) => sin(θ)
        inv(tan(θ)) => cot(θ)
        inv(cot(θ)) => tan(θ)

        # Pythagorean identities
        sin(θ)^2 + cos(θ)^2 => one(θ)
        one(θ) + tan(θ)^2 => sec(θ)^2  # NOTE: will not match any one constants
        one(θ) + cot(θ)^2 => csc(θ)^2

        # Negative angles
        sin(-θ) => -sin(θ)
        cos(-θ) => cos(θ)
        tan(-θ) => -tan(θ)
        csc(-$x) => -csc($x)
        sec(-θ) => sec(θ)
        cot(-$x) => -cot($x)

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
        cos(θ)^2 + -sin(θ)^2 => cos(2θ)
        2cos(θ)^2 + -1 => cos(2θ)

        # Sum and difference formulae
        sin(α)cos(β) + cos(α)sin(β) => sin(α + β)
        sin(α)cos(β) + -cos(α)sin(β) => sin(α - β)
        cos(α)cos(β) + -sin(α)sin(β) => cos(α + β)
        cos(α)cos(β) + sin(α)sin(β) => cos(α - β)
        (tan(α) + tan(β)) * inv(1 + -tan(α)tan(β)) => tan(α + β)
        (tan(α) + -tan(β)) * inv(1 + tan(α)tan(β)) => tan(α - β)

        # Product to sum formulae
        cos(α + -β) + -cos(α + β) => 2sin(α)sin(β)
        cos(α + -β) + cos(α + β) => 2cos(α)cos(β)
        sin(α + β) + sin(α + -β) => 2sin(α)cos(β)
        sin(α + β) + -sin(α + -β) => 2cos(α)sin(β)

        # Sum to product formulae
        2sin((α + β) * inv(2))cos((α + -β) * inv(2)) => sin(α) + sin(β)
        2cos((α + β) * inv(2))sin((α + -β) * inv(2)) => sin(α) - sin(β)
        2cos((α + β) * inv(2))cos((α + -β) * inv(2)) => cos(α) + cos(β)
        -2sin((α + β) * inv(2))sin((α + -β) * inv(2)) => cos(α) - cos(β)

        # Cofunction formulae
        sin(π * inv(2) + -θ) => cos(θ)
        cos(π * inv(2) + -θ) => sin(θ)
        csc(π * inv(2) + -θ) => sec(θ)
        sec(π * inv(2) + -$x) => csc($x)
        tan(π * inv(2) + -$x) => cot($x)
        cot(π * inv(2) + -θ) => tan(θ)
    ]
end

function rules(::Val{:TYPES})
    rules = []

    types = [Number, Int, Float64]
    for T ∈ types
        x = Variable(:x, TypeSet(T))
        push!(rules, @term(zero($x)) => @term(zero($T)))
        push!(rules, @term($x + zero($x)) => @term($x))
        push!(rules, @term($x + $(zero(T))) => @term($x))
        push!(rules, @term(-($(zero(T)))) => @term($(zero(T))))

        push!(rules, @term(one($x)) => @term(one($T)))
        push!(rules, @term($x * one($x)) => @term($x))
        push!(rules, @term(one($x) * $x) => @term($x))
        push!(rules, @term($x * $(one(T))) => @term($x))
        push!(rules, @term($(one(T)) * $x) => @term($x))
        push!(rules, @term(inv($(one(T)))) => @term($(one(T))))

        push!(rules, @term($x * zero($x)) => @term(zero($x)))
        push!(rules, @term(zero($x) * $x) => @term(zero($x)))
        push!(rules, @term($x * $(zero(T))) => @term($(zero(T))))
        push!(rules, @term($(zero(T)) * $x) => @term($(zero(T))))
    end

    TRS(
        rules...,
        EvalRule(zero),
        EvalRule(one),
    )
end
