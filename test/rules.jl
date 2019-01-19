@syms f g
@syms a b c d e
@vars x y z

@testset "Rule" begin

    @testset "Rule" begin
        @test normalize(@term(f(a, 0)), Rule(@term(f(x, 0)), @term(x))) == @term(a)
        @test normalize(@term(a + 0), Rule(@term(x + 0), @term(x))) == @term(a)
        @test normalize(@term(b + 1), Rule(@term(x + 0), @term(x))) == @term(b + 1)
        @test normalize(@term(b), Rule(@term(x + 0), @term(x))) == @term(b)
        @test normalize(@term(f(a, b)), Rules(@term(f(x, y)) => @term(g(x)))) == @term(g(a))
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
