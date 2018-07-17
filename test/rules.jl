using SymReduce: PatternRule, EvalRule

@testset "Rule" begin
    @testset "PatternRule" begin
        @test_broken normalize(@term(x + 0 + 0), PatternRule{Term}(@term(a + 0), @term(a))) == @term(x + 0)
        @test normalize(@term(y + 1), PatternRule{Term}(@term(a + 0), @term(a))) == @term(y + 1)
        @test normalize(@term(y), PatternRule{Term}(@term(a + 0), @term(a))) == @term(y)
        @test normalize(@term(f(a, b)), TRS(@term(f(x, y)) => @term(g(x)))) == @term(g(a))
        @test_throws ArgumentError("Divergent normalization paths") normalize(@term(f(a, b) where {f::C}), TRS(@term(f(x, y) where {f::C}) => @term(g(x))))
        @test_broken normalize(@term(x + 0 + 0), TRS(@term(a + 0) => @term(a))) == @term(x)
        @test normalize(@term(+x), TRS(@term(+a) => @term(a))) == @term(x)
    end
    @testset "EvalRule" begin
        @test normalize(@term(f(2, 3)), EvalRule{Fn{:f,2}}(*)) == @term(6)
        @test normalize(@term(f(x, 3)), EvalRule{Fn{:f,2}}(*)) == @term(f(x, 3))
        @test normalize(@term(f(2, y)), EvalRule{Fn{:f,2}}(*)) == @term(f(2, y))
        @test normalize(@term("a" * "b"), EvalRule{Associative{:*}}(*)) == @term("ab")
        @test normalize(@term(x + 2 * 3), EvalRule{Associative{:*}}(*)) == @term(x + 2 * 3)
        @test normalize(@term(x + 2 * 3), TRS(EvalRule{Associative{:*}}(*))) == @term(x + 6)
        @test normalize(@term(2 * 3 + 4 * 5), TRS(EvalRule{Associative{:*}}(*))) == @term(6 + 20)
        @test normalize(@term(2 * 3 + 4 * 5), TRS(EvalRule{Commutative{Associative{:+}}}(+), EvalRule{Associative{:*}}(*))) == @term(26)
    end
end

@testset "normalize" begin
    @testset "STANDARD" begin
        @test normalize(@term(x)) == @term(x)
        @test normalize(@term(x + 0)) == @term(x)
        @test_broken normalize(@term(y + 0 + 0)) == @term(y)
        @test normalize(@term(y * (1 + 2 - 3))) == @term(0)
        @test_broken normalize(@term(0 + y + 0)) == @term(y)
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
