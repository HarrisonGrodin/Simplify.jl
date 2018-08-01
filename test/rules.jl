using Rewrite: PatternRule, EvalRule

@testset "Rule" begin
    @testset "PatternRule" begin
        @test normalize(@term(x + 0 + 0), PatternRule{Term}(@term(a + 0), @term(a))) == @term(x + 0)
        @test normalize(@term(y + 1), PatternRule{Term}(@term(a + 0), @term(a))) == @term(y + 1)
        @test normalize(@term(y), PatternRule{Term}(@term(a + 0), @term(a))) == @term(y)
        @test normalize(@term(f(a, b)), TRS(@term(f(x, y)) => @term(g(x)))) == @term(g(a))
        @test_throws ArgumentError("Divergent normalization paths") normalize(@term(f(a, b) where {f::C}), TRS(@term(f(x, y) where {f::C}) => @term(g(x))))
        @test normalize(@term(x + 0 + 0), TRS(@term(a + 0) => @term(a))) == @term(x)
    end
    @testset "EvalRule" begin
        @test normalize(@term(f(2, 3)), EvalRule(:f, *)) == @term(6)
        @test normalize(@term(f(x, 3)), EvalRule(:f, *)) == @term(f(x, 3))
        @test normalize(@term(f(2, y)), EvalRule(:f, *)) == @term(f(2, y))
        @test normalize(@term("a" * "b"), EvalRule(*)) == @term("ab")
        @test normalize(@term(x + 2 * 3), EvalRule(*)) == @term(x + 2 * 3)
        @test normalize(@term(x + 2 * 3), TRS(EvalRule(*))) == @term(x + 6)
        @test normalize(@term(2 * 3 + 4 * 5), TRS(EvalRule(*))) == @term(6 + 20)
        @test normalize(@term(2 * 3 + 4 * 5), TRS(EvalRule(+), EvalRule(*))) == @term(26)
    end
end

@testset "normalize" begin
    @testset "STANDARD" begin
        @test normalize(@term(x)) == @term(x)
        @test normalize(@term(x + 0)) == @term(x)
        @test normalize(@term(y + 0 + 0)) == @term(y)
        @test normalize(@term(y * (1 + 2 - 3))) == @term(0)
        @test normalize(@term(0 + y + 0)) == @term(y)
        @test normalize(@term(sin(π/3)cos(0) + cos(π/3)sin(0))) == @term(√3 / 2)
    end

    @testset "ABSOLUTE_VALUE" begin
        @test normalize(@term(abs(-x))) == @term(-abs(x))
        @test normalize(@term(abs(x * y))) == @term(abs(x) * abs(y))
        @test normalize(@term(abs(x / y))) == @term(abs(x) / abs(z))
    end

    @testset "BOOLEAN" begin
        @test normalize(@term(and(x, true))) == @term(x)
        @test normalize(@term(or(x, and(x, y)))) == @term(x)
        @test normalize(@term(or(y, neg(y)))) == @term(true)
        @test normalize(@term(and(y, y))) == @term(y)
        @test normalize(@term(neg(neg(x)))) == @term(x)
    end

    @testset "LOGARITHM" begin
        @test normalize(@term(log(b, x * y))) == @term(log(b, x) + log(b, y))
        @test normalize(@term(log(b, 1))) == @term(0)
        @test normalize(@term(log(b, b ^ x))) == @term(x)
    end

    @testset "TRIGONOMETRY" begin
        @test normalize(@term(sin(0) * tan(π / 4)), :TRIGONOMETRY) == @term(0 * 1)
        @test normalize(@term(one(θ) + tan(θ) ^ 2)) == @term(sec(θ) ^ 2)
        @test normalize(@term(tan(π / 6))) == @term(√3 / 3)
        @test normalize(@term(1 / (sin(-θ) / cos(-θ)))) == @term(cot(θ))
        @test normalize(@term(2 * cos((α + β) / 2) * cos(α - β / 2))) == @term(cos(α) + cos(β))
        @test normalize(@term((tan(α) * tan(β)) / (1 + tan(α) * tan(β)))) == @term(tan(α - β))
        @test normalize(@term(csc(π/2 - θ))) == @term(sec(θ))
    end


    @testset "custom" begin
        @test normalize(@term(f(y, y)), @term RULES [
            f(x, x) => x
        ]) == @term y
        @test normalize(@term(f(x, y)), @term RULES [
            f(x, x) => x
        ]) == @term f(x, y)
        @test normalize(@term(f(f(x), x)), @term RULES [
            f(x, x) => x, f(x) => x
        ]) == @term x
        @test normalize(@term(f(f(x), g(x))), @term RULES [
            f(f(x), g(x)) => x, g(x) => x
        ]) == @term f(f(x), x)
    end
end
