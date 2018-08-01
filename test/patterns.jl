using Rewrite.Patterns
using Rewrite.Patterns: Match, Unify, AlgebraContext
using SpecialSets


@testset "Patterns" begin

    @testset "Variable" begin
        a, b, x, y = Variable.([:a, :b, :x, :y])

        @test x == x
        @test x ≠ y
        @test a == @term a

        @test unify(x, x) == Unify()
        @test unify(x, y) == Unify(x => y)
        @test unify(y, x) == Unify(y => x)

        @test match(a, b) == Match(a => b)
        @test b ⊆ a

        @test replace(a, Dict(a => b)) == b
        @test replace(a, Dict(x => b)) == a

        @testset "Predicates" begin
            @test Variable(:x) ≠ Variable(:x, Even)
            @test Variable(:x, Even) == Variable(:x, Even)
            @test Variable(:x, Set([1])) == Variable(:x, Set([1]))
            @test Variable(:x, Set([1])) ≠ Variable(:x, Set([2]))

            nz, p, n = Variable.([:nz, :p, :n], [Nonzero, Positive, Negative])

            @test match(nz, nz) == Match(nz => nz)
            @test match(@term($nz / $nz), @term(3 / 3)) == Match(nz => @term(3))
            @test match(@term($nz / $nz), @term(2 / 3)) == zero(Match)
            @test match(@term($nz / $nz), p) == zero(Match)
            @test match(@term($nz / $nz), @term($p / $p)) == Match(nz => p)
            @test match(@term($nz / $nz), @term($n / $p)) == zero(Match)
        end
    end

    @testset "Constant" begin
        _1, _2 = Constant(1), Constant(2)

        @test _1 == _1
        @test _1 ≠ _2

        @test _1 == @term 1

        @test unify(_2, _2) == Unify()
        @test unify(Constant(2), Constant(3)) === nothing
        @test unify(Constant(1), Constant("one")) === nothing

        @test match(_1, _1) == one(Match)
        @test _1 ⊆ _1
        @test match(_2, _1) == zero(Match)
        @test _1 ⊈ _2
        @test match(@term(f(x, 0)), @term(f(y, 0))) == Match(@term(x) => @term(y))

        @test replace(_1, Dict(@term(x) => @term(y))) == _1
    end

    @testset "Function" begin
        x, y, z = Variable.([:x, :y, :z])
        f(xs...) = Fn(:f, xs...)
        g(xs...) = Fn(:g, xs...)

        @test f(x) == f(x)
        @test f(x) ≠ f(y)
        @test f(x) == @term f(x)

        @test @term(f(x, y))[1] == @term(x)
        @test_throws BoundsError @term(f(x, y))[3]
        @test @term(f(x, g(h(y), f(z))))[2, 1] == @term(h(y))
        @test @term(f(x, g(h(y), f(z))))[2, 1, 1] == @term(y)
        @test_throws MethodError @term(f(x))[1, 1]

        @test unify(x, f(y)) == Unify(x => f(y))
        @test unify(f(y), x) == Unify(x => f(y))
        @test unify(x, f(x)) == nothing
        @test unify(f(x), f(y)) == Unify(x => y)
        @test unify(f(x), g(y)) == nothing
        @test unify(g(x, x), g(y, z)) == Unify(x => z, y => z)
        @test unify(g(f(x), x), g(f(y), z)) == Unify(x => z, y => z)
        @test unify(g(f(x), x), g(y, y)) == nothing
        @test unify(g(f(x), x), g(y, z)) == Unify(y => f(z), x => z)
        @test unify(f(x), f(x, y)) == nothing

        @test match(f(), f()) == one(Match)
        @test match(f(x), f(y)) == Match(x => y)
        @test f(y) ⊆  f(x)
        @test match(f(x), g(x)) == zero(Match)
        @test match(f(f(), x), f(g(), y)) == zero(Match)
        @test match(f(x, x), f(y, z)) == zero(Match)
        @test match(f(x), g(x, y)) == zero(Match)
        @test match(@term(x - x), @term(-y)) == zero(Match)
        @test g(x, y) ⊈ f(x)
        @test @term(f(a, 2, b)) ⊈ @term(f(x, 2, x))

        @test replace(f(x), Dict(x => y)) == f(y)
        @test replace(f(x), Dict(y => x)) == f(x)
        @test replace(f(x, g(y, z)), Dict(g(y, z) => 0)) == f(x, 0)

        @testset "flat" begin

            with_context(AlgebraContext(Dict(:f => [Flat]))) do
                @test @term(f(w, x, f(y, z))) == @term(f(w, x, y, z))
                @test length(@term(f(w, x, g(y, z)))) == 3

                @test match(@term(f(g(X), g(Y), Z)), @term(f(g(a), g(b), g(c), g(d), g(e)))) ==
                    Match(Dict(@term(X)=>@term(a), @term(Y)=>@term(b), @term(Z)=>@term(f(g(c), g(d), g(e)))))
            end

            @test match(@term(x), @term(a * b)) ==
                Match(@term(x) => @term(a * b))

            @test match(@term(x * y), @term(a() * b()))::Match ==
                Match(Dict(@term(x) => @term(a()), @term(y) => @term(b())))

            @test match(@term(x * y), @term(1 * b)) ==
                Match(Dict(@term(x) => @term(1), @term(y) => @term(b)))

            @test match(@term(x * y), @term(a * b * c)) == Match(
                Dict(@term(x) => @term(a * b), @term(y) => @term(c)),
                Dict(@term(x) => @term(a), @term(y) => @term(b * c)),
            )

            @test match(@term(x * 1 * y), @term(f() * g() * 1 * h())) ==
                Match(Dict(@term(x) => @term(f() * g()), @term(y) => @term(h())))

            @test match(@term(f() * g()), @term(f() * g())) ==
                one(Match)

            @test match(@term(g() * f()), @term(f() * g())) ==
                zero(Match)

            @test replace(@term(w * x * (y * x)), Dict(@term(x) => @term(z))) == @term(w * z * y * z)
            @test_skip replace(@term(x * y * z), Dict(@term(y * z) => @term(2))) == @term(x * 2)

        end

        @testset "commutative" begin

            @testset "standard" begin

                with_context(AlgebraContext(Dict(:f => [Orderless]))) do
                    @test match(@term(f(x, 1)), @term(f(1, y))) ==
                        Match(@term(x) => @term(y))

                    @test match(@term(f(g(x), g(y), z)), @term(f(h(a), g(b), g(c))))::Match == Match(
                        Dict(@term(x) => @term(b), @term(y) => @term(c), @term(z) => @term(h(a))),
                        Dict(@term(x) => @term(c), @term(y) => @term(b), @term(z) => @term(h(a))),
                    )

                    @test replace(@term(f(x, y)), Dict(@term(x) => @term(1))) == @term(f(y, 1))
                    @test replace(@term(f(f(x, y), z)), Dict(@term(f(x, y)) => 2)) == @term(f(z, 2))
                end

            end

            @testset "flat" begin

                with_context(AlgebraContext(Dict(:f => [Flat, Orderless]))) do
                    @test replace(@term(f(x, y, z)), Dict(@term(y) => @term(x^3))) == @term(f(x, x^3, z))
                    @test_skip replace(@term(f(x, y, z)), Dict(@term(f(x, z)) => :w)) == @term(f(w, y))
                end

                with_context(AlgebraContext(Dict(:+ => [Flat, Orderless], :* => [Flat, Orderless]))) do
                    @test @term((x+y+b*a)) == @term((a*b+x+y))
                end

                @test match(@term(x), @term(a + b)) ==
                    Match(@term(x) => @term(a + b))

                @test match(@term(x + y), @term(a() + b()))::Match == Match(
                    Dict(@term(x) => @term(a()), @term(y) => @term(b())),
                    Dict(@term(x) => @term(b()), @term(y) => @term(a())),
                )

                @test match(@term(x + y), @term(1 + b)) == Match(
                    Dict(@term(x) => @term(1), @term(y) => @term(b)),
                    Dict(@term(x) => @term(b), @term(y) => @term(1)),
                )

                @test match(@term(x + y), @term(a + b)) == Match(
                    Dict(@term(x) => @term(a), @term(y) => @term(b)),
                    Dict(@term(x) => @term(b), @term(y) => @term(a)),
                )

                @test length(match(@term(x + y), @term(a + b + c))) == 6

                @test match(@term(x + 0), @term(f() + 0 + g())) == Match(
                    Dict(@term(x) => @term(f() + g())),
                )

                @test match(@term(x + y + 1), @term(f() + 1 + g())) == Match(
                    Dict(@term(x) => @term(f()), @term(y) => @term(g())),
                    Dict(@term(x) => @term(g()), @term(y) => @term(f())),
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

end
