using Rewrite: Match, AlgebraContext, Symbolic
using SpecialSets


@testset "Patterns" begin

	@syms f g h

	@testset "Symbolic" begin
		@test get(@term 1) == 1
		@test get(@term sin) == sin

		@test f == @term f
		@test f ≠ @term :f

		@test_throws UndefVarError @term qwerty
	end

    @testset "Variable" begin
        a, b, x, y = Term.([:a, :b, :x, :y])

        @test x == x
        @test x ≠ y
        @test a == @term a

        @test match(a, b) == Match(a => b)
        @test b ⊆ a

        @test replace(a, Dict(a => b)) == b
        @test replace(a, Dict(x => b)) == a

        # @testset "Predicates" begin
        #     @test Variable(:x) ≠ Variable(:x, Even)
        #     @test Variable(:x, Even) == Variable(:x, Even)
        #     @test Variable(:x, Set([1])) == Variable(:x, Set([1]))
        #     @test Variable(:x, Set([1])) ≠ Variable(:x, Set([2]))
        #
        #     nz, p, n = Term.([:nz, :p, :n], [Nonzero, Positive, Negative])
        #
        #     @test match(nz, nz) == Match(nz => nz)
        #     @test match(@term(x / x, Dict(:x => Nonzero)), @term(3 / 3)) == Match(@term(x) => @term(3))
        #     @test match(@term($nz / $nz), @term(2 / 3)) == zero(Match)
        #     @test match(@term($nz / $nz), p) == zero(Match)
        #     @test match(@term($nz / $nz), @term($p / $p)) == Match(nz => p)
        #     @test match(@term($nz / $nz), @term($n / $p)) == zero(Match)
        # end
    end

    @testset "Constant" begin
        _1, _2 = Term.([1, 2])

        @test _1 == _1
        @test _1 ≠ _2

        @test _1 == @term 1

        @test match(_1, _1) == one(Match)
        @test _1 ⊆ _1
        @test match(_2, _1) == zero(Match)
        @test _1 ⊈ _2


        @test match(@term(f(:x, 0)), @term(f(:y, 0))) == Match(@term(:x) => @term(:y))

        @test replace(_1, Dict(@term(:x) => @term(:y))) == _1
    end

    @testset "Function" begin

        @testset "standard" begin
            x, y, z = Term.([:x, :y, :z])

            @test f(x) == f(x)
            @test f(x) ≠ f(y)
			@test f(x) == @term f(x)
            @test f(x) == @term f(:x)

            @test @term(f(x, y))[2] == @term(x)
            @test_throws BoundsError @term(f(x, 2))[8]
            @test @term(f(x, g(h(y), f(z))))[3, 2] == @term(h(y))
            @test @term(f(x, g(h(y), f(z))))[3, 2, 2] == @term(y)
            @test_throws MethodError @term(f(x))[2, 1]

            @test match(f(), f()) == one(Match)
            @test match(f(x), f(y)) == Match(x => y)
            @test f(y) ⊆  f(x)
            @test match(f(x), g(x)) == zero(Match)
            @test match(f(f(), x), f(g(), y)) == zero(Match)
            @test match(f(x, x), f(y, z)) == zero(Match)
            @test match(f(x), g(x, y)) == zero(Match)
            @test match(@term(x - x), @term(-y)) == zero(Match)
            @test g(x, y) ⊈ f(x)
            @test @term(f(:a, 2, :b)) ⊈ @term(f(x, 2, x))

            @test replace(f(x), Dict(x => y)) == f(y)
            @test replace(f(x), Dict(y => x)) == f(x)
            @test replace(f(x, g(y, z)), Dict(g(y, z) => 0)) == f(x, 0)
            @test replace(f(x, g(y, z)), Dict(g(y, z) => @term(0))) == f(x, 0)
        end

        @testset "flat" begin

			@syms a b c d e

            with_context(AlgebraContext(props=Dict(Symbolic(:f) => [Flat]))) do
                @test match(@term(f(1, 2, f(3, 4))), @term(f(1, 2, 3, 4))) == one(Match)

                @test match(@term(f(g(:X), g(:Y), :Z)), @term(f(g(a), g(b), g(c), g(d), g(e)))) ==
                    Match(Dict(@term(:X)=>@term(a), @term(:Y)=>@term(b), @term(:Z)=>@term(f(g(c), g(d), g(e)))))

                @test match(@term(f(:x, :y)), @term(f(1, b))) ==
                    Match(Dict(@term(:x) => @term(1), @term(:y) => @term(b)))
            end

            with_context(AlgebraContext(props=Dict((*) => [Flat]))) do
                @test match(@term(:x), @term(a * b)) ==
                    Match(@term(:x) => @term(a * b))

                @test match(@term(:x * :y), @term(a() * b()))::Match ==
                    Match(Dict(@term(:x) => @term(a()), @term(:y) => @term(b())))

                @test match(@term(:x * :y), @term(a * b * c)) == Match(
                    Dict(@term(:x) => @term(a * b), @term(:y) => @term(c)),
                    Dict(@term(:x) => @term(a), @term(:y) => @term(b * c)),
                )

                @test match(@term(:x * "_" * :y), @term(f() * g() * "_" * h())) ==
                    Match(Dict(@term(:x) => @term(f() * g()), @term(:y) => @term(h())))

                with_context(AlgebraContext(props=Dict((*) => [Flat]))) do
                    @test match(@term(f() * g()), @term(f() * g())) ==
                        one(Match)

                    @test match(@term(g() * f()), @term(f() * g())) ==
                        zero(Match)
                end

                @test replace(@term(a * b * (c * b)), Dict(@term(b) => @term(d))) == @term(a * d * (c * d))
                @test_broken replace(@term(a * b * c), Dict(@term(b * c) => @term(2))) == @term(a * 2)
            end

        end

    #     @testset "orderless" begin
    #
    #         @testset "standard" begin
    #
    #             with_context(AlgebraContext(props=Dict(:f => [Orderless]))) do
    #                 @test match(@term(f(x, 1)), @term(f(1, y))) ==
    #                     Match(@term(x) => @term(y))
    #
    #                 @test match(@term(f(g(x), g(y), z)), @term(f(h(a), g(b), g(c))))::Match == Match(
    #                     Dict(@term(x) => @term(b), @term(y) => @term(c), @term(z) => @term(h(a))),
    #                     Dict(@term(x) => @term(c), @term(y) => @term(b), @term(z) => @term(h(a))),
    #                 )
    #
    #                 @test replace(@term(f(x, y)), Dict(@term(x) => @term(1))) == @term(f(y, 1))
    #                 @test replace(@term(f(f(x, y), z)), Dict(@term(f(x, y)) => 2)) == @term(f(z, 2))
    #             end
    #
    #         end
    #
    #         @testset "partial" begin
    #             @test match(@term(f() * g()), @term(g() * f())) ==
    #                 one(Match)
    #
    #             @test match(@term(x * y), @term(1 * b)) == Match(
    #                 Dict(@term(x) => @term(1), @term(y) => @term(b)),
    #                 Dict(@term(x) => @term(b), @term(y) => @term(1)),
    #             )
    #
    #             t = @term(3 * (x * 2) * y)
    #             args = collect(t)
    #             @test findfirst(==(@term x), args) < findfirst(==(@term y), args)
    #             @test t == @term(2 * 3 * x * y)
    #         end
    #
    #         @testset "flat" begin
    #
    #             with_context(AlgebraContext(props=Dict(:f => [Flat, Orderless]))) do
    #                 @test replace(@term(f(x, y, z)), Dict(@term(y) => @term(x^3))) == @term(f(x, x^3, z))
    #                 @test_skip replace(@term(f(x, y, z)), Dict(@term(f(x, z)) => :w)) == @term(f(w, y))
    #             end
    #
    #             with_context(AlgebraContext(props=Dict(:+ => [Flat, Orderless], :* => [Flat, Orderless]))) do
    #                 @test @term((x+y+b*a)) == @term((a*b+x+y))
    #             end
    #
    #             @test match(@term(x), @term(a + b)) ==
    #                 Match(@term(x) => @term(a + b))
    #
    #             @test match(@term(x + y), @term(a() + b()))::Match == Match(
    #                 Dict(@term(x) => @term(a()), @term(y) => @term(b())),
    #                 Dict(@term(x) => @term(b()), @term(y) => @term(a())),
    #             )
    #
    #             @test match(@term(x + y), @term(1 + b)) == Match(
    #                 Dict(@term(x) => @term(1), @term(y) => @term(b)),
    #                 Dict(@term(x) => @term(b), @term(y) => @term(1)),
    #             )
    #
    #             @test match(@term(x + y), @term(a + b)) == Match(
    #                 Dict(@term(x) => @term(a), @term(y) => @term(b)),
    #                 Dict(@term(x) => @term(b), @term(y) => @term(a)),
    #             )
    #
    #             @test length(match(@term(x + y), @term(a + b + c))) == 6
    #             @test match(@term(a + b + c), @term(x + y)) == zero(Match)
    #
    #             @test match(@term(x + 0), @term(f() + 0 + g())) == Match(
    #                 Dict(@term(x) => @term(f() + g())),
    #             )
    #
    #             @test match(@term(x + y + 1), @term(f() + 1 + g())) == Match(
    #                 Dict(@term(x) => @term(f()), @term(y) => @term(g())),
    #                 Dict(@term(x) => @term(g()), @term(y) => @term(f())),
    #             )
    #
    #             @test match(@term(f() + g()), @term(f() + g())) ==
    #                 one(Match)
    #
    #             @test match(@term(g() + f()), @term(f() + g())) ==
    #                 one(Match)
    #
    #             @test match(@term(f() + f()), @term(f() + g())) ==
    #                 zero(Match)
    #
    #         end
    #
    #     end

    end

end
