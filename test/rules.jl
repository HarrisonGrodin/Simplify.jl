using SymReduce: PatternRule

@testset "Rule" begin
    @testset "PatternRule" begin
        @test normalize(@term((x + 0) + 0), PatternRule(@term(a + 0), @term(a))) == @term(x + 0)
        @test normalize(@term((x + 0) + 0), [PatternRule(@term(a + 0), @term(a))]) == @term(x)
    end
end

@testset "normalize" begin
    @testset "STANDARD" begin
        @test normalize(@term(x)) == @term(x)
        @test normalize(@term(x + 0)) == @term(x)
        @test normalize(@term((y + 0) + 0)) == @term(y)
        @test_skip normalize(@term(y + 0 + 0)) == @term(y)
        @test normalize(@term(0 + (y + 0))) == @term(y)
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
