using Rewrite: PatternRule, EvalRule, OrderRule


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
                [σ -> !iszero(σ[x])]
            ))
            @test normalize(@term(3 / 3), trs) == @term(one(3))
            @test normalize(@term(2 / 3), trs) == @term(2 / 3)
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

@testset "normalize" begin
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
