using Rewrite: PatternRule, EvalRule
using SpecialSets


@testset "Rule" begin
    @testset "PatternRule" begin
        @test normalize(@term(x + 0 + 0), PatternRule{Term}(@term(a + 0), @term(a))) == @term(x + 0)
        @test normalize(@term(y + 1), PatternRule{Term}(@term(a + 0), @term(a))) == @term(y + 1)
        @test normalize(@term(y), PatternRule{Term}(@term(a + 0), @term(a))) == @term(y)
        @test normalize(@term(f(a, b)), TRS(@term(f(x, y)) => @term(g(x)))) == @term(g(a))
        @test_throws ArgumentError("Divergent normalization paths") normalize(@term(f(a, b) where {f::C}), TRS(@term(f(x, y) where {f::C}) => @term(g(x))))
        @test normalize(@term(x + 0 + 0), TRS(@term(a + 0) => @term(a))) == @term(x)

        @testset "Predicates" begin
            nz = Variable(:nz, Nonzero)
            odd = Variable(:odd, Odd)

            @test normalize(@term(3 / 3), TRS(@term($nz / $nz) => @term(one($nz)))) == @term(one(3))
            @test normalize(@term(2 / 3), TRS(@term($nz / $nz) => @term(one($nz)))) == @term(2 / 3)
            @test normalize(@term(x / x), TRS(@term($nz / $nz) => @term(one($nz)))) == @term(x / x)
            @test normalize(@term((2^x) / (2^x)), TRS(@term($nz / $nz) => @term(one($nz)))) == @term(one(2^x))
            @test normalize(@term($odd / $odd), TRS(@term($nz / $nz) => @term(one($nz)))) == @term(one($odd))
            @test_skip normalize(@term(($odd + 2) / ($odd + 2)), TRS(@term($nz / $nz) => @term(one($nz)))) == @term(one($odd + 2))
        end
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

    @testset "TRIGONOMETRY" begin
        @test normalize(@term(sin(0) * tan(π / 4)), :TRIGONOMETRY) == @term(0 * 1)
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
