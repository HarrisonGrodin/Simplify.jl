using Rewrite: Symbolic, Match


@testset "Patterns" begin

    @syms f g h
    @syms a b c d e
    @vars x y z

    @testset "Symbolic" begin
        @test f == f
        @test f ≠ g
        @test f ≠ @term f
        @test f ≠ @term :f

        @test get(@term 1) == 1
        @test get(@term π) == pi
        @test get(@term im) == im
        @test get(@term sin) == sin

        @test_throws UndefVarError @term qwerty
    end

    @testset "Variable" begin
        @test x == x
        @test x == Variable(:x)
        @test x ≠ y

        @test match(@term(x), @term(y)) == Match(x => y)

        @test replace(@term(x), Match(x => y)) == @term(y)
        @test replace(@term(x), Match(y => x)) == @term(x)
        @test replace(@term(x), Match(y => z)) == @term(x)
    end

    @testset "Constant" begin
        _1, _2 = Term.([1, 2])
        @vars x y

        @test _1 == _1
        @test _1 ≠ _2

        @test _1 == @term 1

        @test match(_1, _1) == Match()
        @test match(_2, _1) == nothing

        @test match(@term(:x), @term(:x)) == Match()
        @test match(@term(:x), @term(:y)) == nothing

        @test match(@term(f(x, 0)), @term(f(y, 0))) == Match(x => y)

        @test replace(_1, Match(x => y)) == _1
    end

    @testset "Function" begin

        @testset "standard" begin

            @test @term(f(x)) == @term(f(x))
            @test @term(f(x)) ≠ @term(f(y))

            @test match(@term(f()), @term(f())) == Match()
            @test match(@term(f(x)), @term(f(y))) == Match(x => y)
            @test match(@term(f(x)), @term(g(x))) == nothing
            @test match(@term(f(f(), x)), @term(f(g(), y))) == nothing
            @test match(@term(f(x, x)), @term(f(y, z))) == nothing
            @test match(@term(f(x)), @term(g(x, y))) == nothing
            @test match(@term(x - x), @term(-y)) == nothing

            @test match(@term(x(y)), @term(sin(3))) == Match(Dict(x => sin, y => 3))
            @test match(@term(map(x, [])), @term(map(iseven, []))) == Match(x => iseven)
            @test match(@term(map(x, [])), @term(map(iseven, [1]))) == nothing

            @test replace(@term(f(x)), Match(x => y)) == @term(f(y))
            @test replace(@term(f(x)), Match(y => x)) == @term(f(x))
        end

    end

    @testset "show" begin
        let
            x = 1
            @test sprint(show, @term([x, :x])) == "@term([1, :x])"
        end
        let
            @syms x
            @test sprint(show, @term(2x + 3)) == "@term(2x + 3)"
        end
        @test sprint(show, @term(Base.Broadcast.materialize(1, 2))) ==
            "@term(Broadcast.materialize(1, 2))"
        let
            @syms a
            @vars x
            @test sprint(show, @term(2^3a+5x)) == "@term($(:(2^3a+5x)))"
        end
    end

    @testset "miscellaneous" begin
        @test match(@term([x, y, z]), @term([a, b, c])) == Match(Dict(
            x => a, y => b, z => c,
        ))
        @test match(@term([x, y, z]), @term([2a, a-1, b])) == Match(Dict(
            x => get(@term 2a), y => get(@term a-1), z => b
        ))
        @test match(@term([x, y, z]), @term([a, b])) == nothing

        @test match(@term(x ? y : z), @term(a ? b : c)) == Match(Dict(
            x => a, y => b, z => c,
        ))
        @test match(@term(x ? y : y), @term(a ? b : b)) == Match(Dict(
            x => a, y => b,
        ))
        @test match(@term(x ? y : y), @term(a ? b : c)) == nothing
    end

end
