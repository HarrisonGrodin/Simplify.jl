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
    args = map(ex.args) do rule
        if rule.head == :call
            @assert length(rule.args) == 3 && rule.args[1] == :(=>)
            pair = rule
            ps = []
        else
            @assert rule.head == :where
            pair = rule.args[1]
            ps = Expr(:vect, rule.args[2:end]...)
        end

        @assert pair.head == :call
        p, a, b = pair.args
        @assert p == :(=>)

        esc(:($PatternRule(@term($a), @term($b), $ps)))
    end
    :(Rules($Rule[$(args...)]))
end


rules(set::Symbol, args...; kwargs...) = rules(Val(set), args...; kwargs...)


rules() = [
    rules(:BASIC)
    rules(:ABSOLUTE_VALUE)
    rules(:BOOLEAN)
    rules(:EXP)
    rules(:CALCULUS)
    rules(:LOGARITHM)
    rules(:TRIGONOMETRY)
    rules(:TYPES)
]

function rules(::Val{:BASIC})
    @vars x y z
    @vars f

    [
        @term RULES [
            (f(f(x, y), z) => f(x, y, z)) where {σ -> isvalid(Associative(σ[f]))}
            (f(x, f(y, z)) => f(x, y, z)) where {σ -> isvalid(Associative(σ[f]))}

            x - y  => x + -y
            (x / y  => x * inv(y)) where {_image(y, Nonzero)}

            x + -x     => zero(x)
            (x * inv(x) => one(x)) where {_image(x, Nonzero)}

            -(x + y)   => -y + -x
            (inv(x * y) => inv(y) * inv(x)) where {_image(x, Nonzero)}

            -(-x)      => x
            (inv(inv(x)) => x) where {_image(x, Nonzero)}

            -1 * x     => -x
            -x * y     => -(x * y)
            (inv(-x)   => -inv(x)) where {_image(x, Nonzero)}

            x ^ 0      => one(x)
            x ^ 0.0    => one(x)
            x ^ 1      => x
            x ^ 1.0    => x
        ]
        Rules(
            OrderRule(x -> sprint(show, x)),
            EvalRule(+),
            EvalRule(-),
            EvalRule(*),
        )
    ]
end


function rules(::Val{:ABSOLUTE_VALUE})
    @vars x y

    [
        @term RULES [
            (abs(x)     =>  x)  where {_image(x, Nonnegative)}
            (abs(x)     => -x)  where {_image(x, Negative)   }
            abs(-x)     => abs(x)
            abs(x * y)  => abs(x) * abs(y)
            abs(inv(x)) => inv(abs(x))
        ]
        Rules(
            EvalRule(abs),
        )
    ]
end


function rules(::Val{:BOOLEAN}; and=&, or=|, xor= ⊻, neg=!)
    @vars x y z
    _bool(xs...) = σ -> all(x -> isvalid(Image(σ[x], TypeSet(Bool))), xs)

    [
        @term RULES [
            (and(x, and(y, z)) => and(x, y, z))  where {_bool(x, y, z)}
            (and(and(x, y), z) => and(x, y, z))  where {_bool(x, y, z)}

            (or(x, or(y, z)) => or(x, y, z))  where {_bool(x, y, z)}
            (or(or(x, y), z) => or(x, y, z))  where {_bool(x, y, z)}

            (xor(x, xor(y, z)) => xor(x, y, z))  where {_bool(x, y, z)}
            (xor(xor(x, y), z) => xor(x, y, z))  where {_bool(x, y, z)}

            (xor(x, false) => x)  where {_bool(x)}            
            (or(x, false)  => x)  where {_bool(x)}
            (and(x, true)  => x)  where {_bool(x)}

            (xor(x, true)  => neg(x))  where {_bool(x)}            
            (or(x, true)   => true )   where {_bool(x)}
            (and(x, false) => false)   where {_bool(x)}
            
            (xor(x, x) => false)  where {_bool(x)}
            (or(x, x)  => x)      where {_bool(x)}
            (and(x, x) => x)      where {_bool(x)}

            (or(x, and(x, y)) => x)  where {_bool(x, y)}
            (and(x, or(x, y)) => x)  where {_bool(x, y)}

            (xor(x, neg(x)) => true )  where {_bool(x)}
            (or(x, neg(x))  => true )  where {_bool(x)}
            (and(x, neg(x)) => false)  where {_bool(x)}

            (neg(neg(x)) => x)  where {_bool(x)}
        ];
        Rules(
            EvalRule(xor, ⊻),
            EvalRule(and, &),
            EvalRule(or,  |),
            EvalRule(neg, !),
        );
    ]
end


function _diff(M, fn, arity)
    M === :Base || return
    args = [Variable(Symbol(:_, i)) for i ∈ 1:arity]
    x = Variable(:x)
    f = Expr(:call, getproperty(Base, fn), args...)

    partials = DiffRules.diffrule(M, fn, args...)

    arity == 1 && (partials = (partials,))
    partials = map(x -> get(eval(:(@term $x))), partials)

    ps = (:($*($p, $diff($a, $x))) for (p, a) ∈ zip(partials, args))
    rhs = arity == 1 ? first(ps) : Expr(:call, +, ps...)

    @term(diff(f, x)) => Term(rhs)
end
function rules(::Val{:CALCULUS})
    rules = Rules()

    for (M, fn, arity) ∈ DiffRules.diffrules()
        try
            rule = _diff(M, fn, arity)
            rule === nothing && continue
            push!(rules, rule)
        catch
        end
    end

    @vars x

    push!(rules, @term(diff(x, x)) => @term(one(x)))
    push!(rules, DiffRule())

    rules
end


function rules(::Val{:LOGARITHM})
    n1  = NotEqual(1)
    n01 = NotEqual(0, 1)

    @vars x y r a b c

    @term RULES [
        (log(b, b) => 1)  where {_image(b, n01)}
        (log(b, 1) => 0)  where {_image(b,  n1)}

        (log(b, b ^ x) => x)  where {_image(b, n01)}
        b ^ log(b, x) => x

        log(b, x ^ r) => r * log(b, x)

        log(b, x * y) => log(b, x) + log(b, y)
        log(b, inv(x)) => -log(b, x)

        log(a, b) * log(b, c) => log(a, c)
    ]
end

function rules(::Val{:EXP})
    @vars α β n
    @term RULES [
        exp(2π * im) => 1
        exp(π * im) => -1
        exp(π * im * inv(2)) => im
        exp(π * im * inv(3)) => 1 / 2 + √3 / 2 * im
        exp(π * im * inv(4)) => √2 / 2 + √2 / 2 * im
        exp(π * im * inv(6)) => √3 / 2 + 1 / 2 * im

        exp(α) * exp(β) => exp(α + β)
        exp(α)^n => exp(α * n)
    ]
end

function rules(::Val{:TRIGONOMETRY})
    @vars α β θ

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
        (csc(-θ) => -csc(θ))  where {_image(θ, Nonzero)}
        sec(-θ) => sec(θ)
        (cot(-θ) => -cot(θ))  where {_image(θ, Nonzero)}

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
        sin(α)cos(β) + -(cos(α)sin(β)) => sin(α - β)
        cos(α)cos(β) + -(sin(α)sin(β)) => cos(α + β)
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
        (sec(π * inv(2) + -θ) => csc(θ))  where {_image(θ, Nonzero)}
        (tan(π * inv(2) + -θ) => cot(θ))  where {_image(θ, Nonzero)}
        cot(π * inv(2) + -θ) => tan(θ)
    ]
end

function rules(::Val{:TYPES})
    @vars x

    rs = []
    types = [Number, Int, Float64]
    for T ∈ types
        T_ = TypeSet(T)

        push!(rs, @term RULES [
            (zero(x)        => zero(T)   ) where {_image(x, T_)}
            (x + zero(x)    => x         ) where {_image(x, T_)}
            (x + $(zero(T)) => x         ) where {_image(x, T_)}
            (-($(zero(T)))  => $(zero(T)))

            (one(x)         => one(T)    ) where {_image(x, T_)}
            (x * one(x)     => x         ) where {_image(x, T_)}
            (x * $(one(T))  => x         ) where {_image(x, T_)}
            (one(x) * x     => x         ) where {_image(x, T_)}
            ($(one(T)) * x  => x         ) where {_image(x, T_)}
            (inv($(one(T))) => $(one(T)) )

            (x * zero(x)    => zero(x)   ) where {_image(x, T_)}
            (x * $(zero(T)) => $(zero(T))) where {_image(x, T_)}
            (zero(x) * x    => zero(x)   ) where {_image(x, T_)}
            ($(zero(T)) * x => $(zero(T))) where {_image(x, T_)}
        ])
    end

    [
        rs...
        Rules(
            EvalRule(zero),
            EvalRule(one),
        )
    ]
end
