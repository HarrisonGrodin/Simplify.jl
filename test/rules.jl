using Rewrite: PatternRule, EvalRule, OrderRule
using Rewrite: diff
using SpecialSets


@syms f g
@syms a b c d e
@vars x y z

@testset "Rule" begin

    @testset "PatternRule" begin
        @test normalize(@term(f(a, 0)), PatternRule(@term(f(x, 0)), @term(x))) == @term(a)
        @test normalize(@term(a + 0), PatternRule(@term(x + 0), @term(x))) == @term(a)
        @test normalize(@term(b + 1), PatternRule(@term(x + 0), @term(x))) == @term(b + 1)
        @test normalize(@term(b), PatternRule(@term(x + 0), @term(x))) == @term(b)
        @test normalize(@term(f(a, b)), Rules(@term(f(x, y)) => @term(g(x)))) == @term(g(a))

        @testset "Predicates" begin
            trs = Rules(PatternRule(
                @term(x / x),
                @term(one(x)),
                [σ -> isvalid(Image(σ[x], Nonzero))]
            ))
            odd = Symbolic(:odd)

            with_context([get_context(); Image(x, TypeSet(Number)); Image(odd, Odd)]) do
                @test normalize(@term(3 / 3)        , trs) == @term(one(3))
                @test normalize(@term(2 / 3)        , trs) == @term(2 / 3)
                @test normalize(@term(x / x)        , trs) == @term(x / x)
                @test normalize(@term((2^x) / (2^x)), trs) == @term(one(2^x))
                @test normalize(@term(odd / odd)    , trs) == @term(one(odd))
                @test_skip normalize(@term((odd + 2) / (odd + 2)), trs) == @term(one(odd + 2))
            end

            @vars h
            associative_rs = @term RULES [
                (h(h(x, y), z) => h(x, y, z)) where {σ -> isvalid(Associative(σ[h]))}
                (h(x, h(y, z)) => h(x, y, z)) where {σ -> isvalid(Associative(σ[h]))}
            ]
            @test normalize(@term(a * (b * c)), associative_rs) == @term(a * b * c)
            @test_skip normalize(@term(((a + b) + c) * (d * e)), associative_rs) == @term((a + b + c) * d * e)
            with_context(Context(Associative(f))) do
                @test normalize(@term(a * (b * c))  , associative_rs) == @term(a * (b * c))
                @test normalize(@term(f(f(a, b), c)), associative_rs) == @term(f(a, b, c))
                @test_skip normalize(@term(f(f(1, f(2, 3), 4), f(5, f(6, 7)))), associative_rs) ==
                    @term(f(1, 2, 3, 4, 5, 6, 7))
            end
        end
    end
    @testset "EvalRule" begin
        @test normalize(@term(f(2, 3)), EvalRule(f, *)) == @term(6)
        @test normalize(@term(f(a, 3)), EvalRule(f, *)) == @term(f(a, 3))
        @test normalize(@term(f(2, b)), EvalRule(f, *)) == @term(f(2, b))
        @test normalize(@term("a" * "b"), EvalRule(*)) == @term("ab")
        @test normalize(@term(a + 2 * 3), EvalRule(*)) == @term(a + 2 * 3)
        @test normalize(@term(a + 2 * 3), Rules(EvalRule(*))) == @term(a + 6)
        @test normalize(@term(2 * 3 + 4 * 5), Rules(EvalRule(*))) == @term(6 + 20)
        @test normalize(@term(2 * 3 + 4 * 5), Rules(EvalRule(+), EvalRule(*))) == @term(26)

        with_context(Context(Associative(f))) do
            rule = EvalRule(f, +)
            @test normalize(@term(f(a, 1, 2, b, 3, c)), rule) == @term(f(a, 3, b, 3, c))
            @test normalize(@term(f(1, 2, 3, 4, 5)), rule) == @term(15)
            @test normalize(@term(f(1, 2, x, 3, 4, 5)), rule) == @term(f(3, x, 12))
            @test normalize(@term(f(1, 2, x, y, 3, 4, 5)), rule) == @term(f(3, x, y, 12))
        end

        with_context(Context(Associative(f), Commutative(f))) do
            rule = EvalRule(f, +)
            @test normalize(@term(f(a, 1, 2, b, 3, c)), rule) == @term(f(a, b, c, 6))
            @test normalize(@term(f(1, 2, 3, 4, 5)), rule) == @term(15)
            @test normalize(@term(f(1, 2, x, 3, 4, 5)), rule) == @term(f(x, 15))
            @test normalize(@term(f(1, 2, x, y, 3, 4, 5)), rule) == @term(f(x, y, 15))
        end
    end
    @testset "OrderRule" begin
        with_context(Context(Commutative(f))) do
            rule = OrderRule(x -> sprint(show, x))
            @test normalize(@term(f(a, b)), rule) == @term(f(a, b))
            @test normalize(@term(f(b, a)), rule) == @term(f(a, b))
            @test normalize(@term(f(2, 1)), rule) == @term(f(1, 2))
            @test normalize(@term(f(1, 1.0)), rule) == @term(f(1, 1.0))
            @test normalize(@term(f(1.0, 1)), rule) == @term(f(1, 1.0))
        end
    end
end


@testset "accuracy of standard rules" begin
    @testset "$set" for set ∈ [:BASIC, :ABSOLUTE_VALUE, :BOOLEAN, :CALCULUS, :LOGARITHM, :TRIGONOMETRY, :TYPES]
        CASES = Any[-10:10; -5:0.1:5; false:true]

        set ∈ [:CALCULUS, :LOGARITHM] && continue

        for rule ∈ rules(set)
            rule isa PatternRule || continue
            l, r = rule.left, rule.right

            @testset "$rule" begin
                for case ∈ CASES
                    vars = Rewrite.vars(l)

                    σ = Dict(var => case for var ∈ vars)
                    Rewrite._preds_match(rule.ps, σ) || continue

                    lres = replace(l, σ) |> get |> eval
                    rres = replace(r, σ) |> get |> eval

                    success = isapprox(lres, rres, atol = 1e-9)
                    success || @error "Case" case
                    @test success
                end
            end
        end
    end
end


@testset "normalize" begin

    @testset "BASIC" begin
        @syms a b

        with_context([get_context(); Image.([a, b], Ref(TypeSet(Number)))]) do
            @test normalize(@term(37)) == @term(37)
            @test normalize(@term(a)) == @term(a)
            @test normalize(@term(a + 0)) == @term(a)
            @test normalize(@term(b + 0 + 0)) == @term(b)
            @test normalize(@term(0 + 0 + b)) == @term(b)
            @test normalize(@term(b * (1 + 2 - 3))) == @term(0)
            @test normalize(@term(0 + b + 0)) == @term(b)
            @test normalize(@term(a * (2 * b))) == @term(a * 2 * b)
            @test normalize(@term(a * (b * c))) == @term(a * b * c)
            @test normalize(@term((a * b) * c)) == @term(a * b * c)
            @test_broken normalize(@term(a * ((b * c) * d))) == @term(a * b * c * d)
            @test_broken normalize(@term(a * ((b * c) * d) * e)) == @term(a * b * c * d * e)
            @test normalize(@term(a + ((2 + b) + 3))) == @term(a + b + 5)
            @test normalize(@term(a + 2)) == normalize(@term(2 + a))
        end
    end

    @testset "ABSOLUTE_VALUE" begin
        @syms a b

        with_context([get_context(); Image(a, TypeSet(Int)); Image(b, Nonzero)]) do
            @test normalize(@term(abs(a))) == @term(abs(a))
            @test normalize(@term(abs(-a))) == @term(abs(a))
            @test normalize(@term(abs(0))) == @term(0)
            @test normalize(@term(abs(-3))) == @term(3)
            @test normalize(@term(abs(3))) == @term(3)
            @test normalize(@term(abs(2a))) == @term(2abs(a))
            @test normalize(@term(abs(-(5a)))) == @term(5abs(a))
            @test normalize(@term(abs(a * b))) == @term(abs(a) * abs(b))
            @test normalize(@term(abs(a / b))) == @term(abs(a) * inv(abs(b)))
            @test normalize(@term(abs(a / 1))) == @term(abs(a))
            @test normalize(@term(abs(abs(a)))) == @term(abs(a))
            @test normalize(@term(abs(a^2))) == @term(a^2)
        end

        @syms d1 d2
        with_context([get_context(); Image(d1, Set([1, 2])); Image(d2, Set([-1, 1]))]) do
            @test normalize(@term(abs(d1))) == @term(d1)
            @test normalize(@term(abs(d2))) == @term(abs(d2))
        end
    end

    @testset "BOOLEAN" begin
        @syms x y

        with_context([get_context(); Image.([x, y], Ref(TypeSet(Bool)))]) do
            @test normalize(@term(x & true)) == @term(x)
            @test normalize(@term(x | (x & y))) == @term(x)
            @test normalize(@term(y | !y)) == @term(true)
            @test normalize(@term(!y | y)) == @term(true)
            @test normalize(@term(y & y)) == @term(y)
            @test normalize(@term(!(!x))) == @term(x)
            @test normalize(@term(!(!x & !x))) == @term(x)
            @test normalize(@term(!(!x & !x) & !x)) == @term(false)
            @test normalize(@term(!(!x & !x) | x)) == @term(x)
            @test normalize(@term(!x & x | (y & (y | true)))) == @term(y)
        end
    end

    @testset "CALCULUS" begin
        @syms x y z

        ctx = [
            Image.([x, y, z], Ref(TypeSet(Int)))
            Signature(f, [TypeSet(Number)], TypeSet(Number))
        ]

        with_context([get_context(); ctx]) do
            @test normalize(@term diff(2, x)) == @term(0)
            @test normalize(@term diff(x * y, x)) == @term(x*diff(y, x) + y)
            @test normalize(@term diff(sin(2x + 3y), x)) == @term(cos(2x + 3y) * (3diff(y, x) + 2))
            @test normalize(@term diff(x * y + sin(x^z), x)) ==
                  normalize(@term(y + x*diff(y, x) + cos(x^z)*(z*x^(z-1) + (x^z * log(x) * diff(z, x)))))
            @test normalize(@term diff(2x + tan(x), x)) == @term(tan(x)^2 + 3)
            @test normalize(@term diff(f(x) + 3x, x)) == @term(diff(f(x), x) + 3)
        end

        @syms w
        with_context([get_context(); Image(w, Nonzero ∩ TypeSet(Float64))]) do
            @test normalize(@term diff(log(w), w)) == @term(inv(w))
        end
    end

    @testset "LOGARITHM" begin
        @syms b x y n

        ctx = Context(
            Associative(+),
            Commutative(+),
            Associative(*),
            Commutative(*),
            Image(b, TypeSet(Real)),
            Image(x, TypeSet(Real)),
            Image(y, TypeSet(Real)),
            Image(n, GreaterThan(3)),
        )

        with_context([get_context(); ctx]) do
            @test normalize(@term(log(b, x * y))) == @term(log(b, x) + log(b, y))
            @test normalize(@term(log(n, 1))) == @term(0)
            @test normalize(@term(log(n, n ^ x))) == @term(x)
            @test normalize(@term(log(n, n * y))) == @term(log(n, y) + 1)
            @test normalize(@term(log(n, y * n))) == @term(log(n, y) + 1)
            @test normalize(@term(log(b, 1/n))) == @term(-log(b, n))
            @test_skip normalize(@term(b ^ log(b, x*b))) == @term(x * b)
            @test_skip normalize(@term(b ^ (log(b, x) + log(b, b)))) == @term(x * b)
        end
    end

    @testset "TRIGONOMETRY" begin
        @syms α β θ

        @test normalize(@term(sin(0) * tan(π / 4))) == @term(0)
        @test normalize(@term(sin(π/3)cos(0) + cos(π/3)sin(0))) == @term(√3 * inv(2))
        @test normalize(@term(one(θ) + tan(θ) ^ 2)) == @term(sec(θ) ^ 2)
        @test normalize(@term(tan(π / 6))) == @term(√3 * inv(3))
        @test_broken normalize(@term(1 / (sin(-3) / cos(-3)))) == @term(-cot(3))
        @test normalize(@term(2 * cos((α + β) / 2) * cos((α - β) / 2))) == @term(cos(α) + cos(β))
        @test normalize(@term((tan(α) - tan(β)) * inv(1 + tan(α) * tan(β)))) == @term(tan(-β + α))
        @test normalize(@term(csc(π/2 - θ))) == @term(sec(θ))

        with_context(Context(Image(a, TypeSet(Int)))) do
            @test_broken normalize(@term sin(a)^2 + cos(a)^2 + 1) == 2
        end
    end


    @testset "custom" begin
        @syms a
        @vars x y

        @test normalize(@term(f(a, a)), @term RULES [
            f(x, x) => x
        ]) == @term(a)

        @test normalize(@term(f(a, b)), @term RULES [
            f(x, x) => x
        ]) == @term(f(a, b))

        @test normalize(@term(f(f(a), a)), @term RULES [
            f(x, x) => x
            f(x)     => x
        ]) == @term(a)

        @test normalize(@term(f(f(a), g(b))), @term RULES [
            f(f(x), y) => y
            g(x) => x
        ]) == @term(b)

        @test normalize(@term(f(a, a)), @term RULES [
            f(b, b) => b
        ]) == @term(f(a, a))
    end
end
