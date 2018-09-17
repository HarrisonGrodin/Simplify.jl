using Rewrite: Symbolic, Match, AlgebraContext
using SpecialSets


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
        @test x ≠ y
        @test x ≠ Variable(:x)

        @test match(@term(x), @term(y)) == Match(x => y)
        @test @term(y) ⊆ @term(x)

        @test replace(@term(x), Dict(x => y)) == @term(y)
        @test replace(@term(x), Dict(y => x)) == @term(x)
        @test replace(@term(x), Dict(y => z)) == @term(x)

        @testset "Predicates" begin
            nz = Variable(:nz, Nonzero)
            p  = Variable(:p, Positive)
            n  = Variable(:n, Negative)

            @test match(@term(nz), @term(nz))         == Match(nz => nz)
            @test match(@term(nz / nz), @term(3 / 3)) == Match(nz => 3)
            @test match(@term(nz / nz), @term(2 / 3)) == zero(Match)
            @test match(@term(nz / nz), @term(p))     == zero(Match)
            @test match(@term(nz / nz), @term(p / p)) == Match(nz => p)
            @test match(@term(nz / nz), @term(n / p)) == zero(Match)
        end
    end

    @testset "Constant" begin
        _1, _2 = Term.([1, 2])
        @vars x y

        @test _1 == _1
        @test _1 ≠ _2

        @test _1 == @term 1

        @test match(_1, _1) == one(Match)
        @test _1 ⊆ _1
        @test match(_2, _1) == zero(Match)
        @test _1 ⊈ _2

        @test match(@term(:x), @term(:x)) == one(Match)
        @test match(@term(:x), @term(:y)) == zero(Match)

        @test match(@term(f(x, 0)), @term(f(y, 0))) == Match(x => y)

        @test replace(_1, Dict(x => y)) == _1
    end

    @testset "Function" begin

        @testset "standard" begin

            @test @term(f(x)) == @term(f(x))
            @test @term(f(x)) ≠ @term(f(y))

            @test match(@term(f()), @term(f())) == one(Match)
            @test match(@term(f(x)), @term(f(y))) == Match(x => y)
            @test @term(f(y)) ⊆ @term(f(x))
            @test match(@term(f(x)), @term(g(x))) == zero(Match)
            @test match(@term(f(f(), x)), @term(f(g(), y))) == zero(Match)
            @test match(@term(f(x, x)), @term(f(y, z))) == zero(Match)
            @test match(@term(f(x)), @term(g(x, y))) == zero(Match)
            @test match(@term(x - x), @term(-y)) == zero(Match)
            @test @term(g(x, y)) ⊈ @term(f(x))
            @test @term(f(:a, 2, :a)) ⊆ @term(f(x, 2, x))
            @test @term(f(:a, 2, :b)) ⊈ @term(f(x, 2, x))

            @test match(@term(x(y)), @term(sin(3))) == Match(Dict(x => sin, y => 3))
            @test match(@term(map(x, [])), @term(map(iseven, []))) == Match(x => iseven)
            @test match(@term(map(x, [])), @term(map(iseven, [1]))) == zero(Match)

            @test replace(@term(f(x)), Dict(x => y)) == @term(f(y))
            @test replace(@term(f(x)), Dict(y => x)) == @term(f(x))
        end

        @testset "flat" begin

            with_context(AlgebraContext(props=Dict(f => [Flat]))) do
                @test match(@term(f(1, 2, f(3, 4))), @term(f(1, 2, 3, 4))) == one(Match)

                @test match(@term(f(g(x), g(y), z)), @term(f(g(a), g(b), g(c), g(d), g(e)))) ==
                    Match(Dict(x => a, y => b, z => f(g(c), g(d), g(e))))

                @test match(@term(f(x, y)), @term(f(1, b))) ==
                    Match(Dict(x => 1, y => b))
            end

            with_context(AlgebraContext(props=Dict((*) => [Flat]))) do
                @test match(@term(x), @term(a * b)) ==
                    Match(x => get(@term a * b))

                @test match(@term(x * y), @term(a * b))::Match ==
                    Match(Dict(x => a, y => b))

                @test match(@term(x * y), @term(a * b * c)) == Match(
                    Dict(x => get(@term a * b), y => c),
                    Dict(x => a, y => get(@term b * c)),
                )

                @test match(@term(x * "_" * y), @term(a * b * "_" * c)) ==
                    Match(Dict(x => get(@term a * b), y => c))

                with_context(AlgebraContext(props=Dict((*) => [Flat]))) do
                    @test match(@term(a * b), @term(a * b)) == one(Match)
                    @test match(@term(a * b), @term(b * a)) == zero(Match)
                end

                @test replace(@term(a * b * (c * b)), Dict(b => d)) == @term(a * d * (c * d))
                @test_broken replace(@term(a * b * c), Dict(get(@term(b * c)) => 2)) == @term(a * 2)
            end

        end

        @testset "orderless" begin

            @testset "standard" begin

                with_context(AlgebraContext(props=Dict(f => [Orderless]))) do
                    @test match(@term(f(x, 1)), @term(f(1, y))) == Match(x => y)

                    @test match(@term(f(g(x), g(y), z)), @term(f(h(a), g(b), g(c))))::Match == Match(
                        Dict(x => b, y => c, z => h(a)),
                        Dict(x => c, y => b, z => h(a)),
                    )

                    @test replace(@term(f(x, y)), Dict(x => 1)) == @term(f(1, y))
                    @test replace(@term(f(f(x, y), z)), Dict(f(x, y) => 2)) == @term(f(2, z))
                end

            end

            @testset "partial" begin
                @test_broken match(@term(f() * g()), @term(g() * f())) ==
                    one(Match)

                @test_broken match(@term(x * y), @term(1 * b)) == Match(
                    Dict(x => 1, y => b),
                    Dict(x => b, y => 1),
                )

                @test_broken normalize(@term(3 * (x * 2) * y)) == @term(2 * 3 * x * y)
            end

            @testset "flat" begin

                with_context(AlgebraContext(props=Dict(:f => [Flat, Orderless]))) do
                    @test replace(@term(f(x, y, z)), Dict(y => get(@term(x^3)))) == @term(f(x, x^3, z))
                    @test_skip replace(@term(f(x, y, z)), Dict(f(x, z) => 1)) == @term(f(1, y))
                end

                with_context(AlgebraContext(props=Dict((+) => [Flat, Orderless], (*) => [Flat, Orderless]))) do
                    @test normalize(@term((x+y+b*a))) == normalize(@term((a*b+x+y)))
                end

                @test match(@term(x), @term(a + b)) ==
                    Match(x => get(@term(a + b)))

                @test match(@term(x + y), @term(a() + b()))::Match == Match(
                    Dict(x => a(), y => b()),
                    Dict(x => b(), y => a()),
                )

                @test match(@term(x + y), @term(1 + b)) == Match(
                    Dict(x => 1, y => b),
                    Dict(x => b, y => 1),
                )

                @test match(@term(x + y), @term(a + b)) == Match(
                    Dict(x => a, y => b),
                    Dict(x => b, y => a),
                )

                @test length(match(@term(x + y), @term(a + b + c))) == 12
                @test match(@term(a + b + c), @term(x + y)) == zero(Match)

                @test match(@term(x + 0), @term(f() + 0 + g())) == Match(
                    Dict(x => get(@term(f() + g()))),
                    Dict(x => get(@term(g() + f()))),
                )

                @test match(@term(x + y + 1), @term(a + 1 + b)) == Match(
                    Dict(x => a, y => b),
                    Dict(x => b, y => a),
                )

                @test match(@term(f() + g()), @term(f() + g())) ==
                    one(Match)

                @test match(@term(g() + f()), @term(f() + g())) ==
                    one(Match)

                @test match(@term(f() + f()), @term(f() + g())) ==
                    zero(Match)

            end

        end

    end

    @testset "miscellaneous" begin
        let x = 1
            @test sprint(show, @term([x, :x])) == "@term([1, :x])"
        end

        @test match(@term([x, y, z]), @term([a, b, c])) == Match(Dict(
            x => a, y => b, z => c,
        ))
        @test match(@term([x, y, z]), @term([2a, a-1, b])) == Match(Dict(
            x => get(@term 2a), y => get(@term a-1), z => b
        ))
        @test match(@term([x, y, z]), @term([a, b])) == zero(Match)

        @test match(@term(x ? y : z), @term(a ? b : c)) == Match(Dict(
            x => a, y => b, z => c,
        ))
        @test match(@term(x ? y : y), @term(a ? b : b)) == Match(Dict(
            x => a, y => b,
        ))
        @test match(@term(x ? y : y), @term(a ? b : c)) == zero(Match)
    end

end
