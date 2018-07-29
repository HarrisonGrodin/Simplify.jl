using Rewrite.Patterns
using Rewrite.Patterns: Match, Unify

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
        @test match(_2, _1) |> isempty
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
        @test match(f(x), g(x)) |> isempty
        @test match(f(f(), x), f(g(), y)) |> isempty
        @test match(f(x, x), f(y, z)) |> isempty
        @test match(f(x), g(x, y)) |> isempty
        @test g(x, y) ⊈ f(x)
        @test @term(f(a, 2, b)) ⊈ @term(f(x, 2, x))

        @test replace(f(x), Dict(x => y)) == f(y)
        @test replace(f(x), Dict(y => x)) == f(x)
        @test replace(f(x, g(y, z)), Dict(g(y, z) => 0)) == f(x, 0)
    end

    @testset "Associative" begin
        a = Associative(:+, Associative(:+, @term(x), @term(y)), @term(z))
        @test a == Associative(:+, @term(x), @term(y), @term(z))

        @test @term(f(w, x, f(y, z)) where {f::A}) == @term(f(w, x, y, z) where {f::A})
        @test length(@term(f(w, x, g(y, z)) where {f::A,g::A})) == 3

        @test @term(f(x) where {f::A}) == @term(x)

        @test match(@term(x), a) ==
            Match(@term(x) => a)

        @test match(@term(x), Associative(:+, @term(a), @term(b))) ==
            Match(@term(x) => Associative(:+, @term(a), @term(b)))

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

        @test match(@term(f(g(X), g(Y), Z) where {f::A}), @term(f(g(a), g(b), g(c), g(d), g(e)) where {f::A})) ==
            Match(Dict(@term(X)=>@term(a), @term(Y)=>@term(b), @term(Z)=>@term(f(g(c), g(d), g(e)) where {f::A})))

        @test replace(@term(w * x * (y * x)), Dict(@term(x) => @term(z))) == @term(w * z * y * z)
        @test_skip replace(@term(x * y * z), Dict(@term(y * z) => @term(2))) == @term(x * 2)
    end

    @testset "Commutative" begin

        @testset "Fn" begin
            c = Commutative(Fn(:f, @term(x), @term(y)))
            @test c == @term(f(x, y) where {f::C})

            @test @term(f(f(x, y), z) where f::C)[1] isa Commutative{Fn}

            @test match(@term(x), c) ==
                Match(@term(x) => c)

            @test match(@term(f(x, 1) where {f::C}), @term(f(1, y) where {f::C})) ==
                Match(@term(x) => @term(y))

            @test match(@term(f(g(x), g(y), z) where {f::C}), @term(f(h(a), g(b), g(c)) where {f::C})) == Match(
                Dict(@term(x) => @term(b), @term(y) => @term(c), @term(z) => @term(h(a))),
                Dict(@term(x) => @term(c), @term(y) => @term(b), @term(z) => @term(h(a))),
            )

            @test replace(@term(f(x, y) where f::C), Dict(@term(x) => @term(1))) == @term(f(y, 1) where f::C)
            @test replace(@term(f(f(x, y), z) where f::C), Dict(@term(f(x, y) where f::C) => 2)) == @term(f(z, 2) where f::C)
        end

        @testset "Associative" begin
            ac = Commutative(Associative(:f, @term(x), @term(y), @term(z)))
            @test ac == @term(f(x, y, z) where {f::AC})

            @test @term((x+y+b*a) where {(+)::AC,(*)::AC}) == @term((a*b+x+y) where {(+)::AC,(*)::AC})

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

            @test replace(@term(f(x, y, z) where f::AC), Dict(@term(y) => @term(x^3))) == @term(f(x, x^3, z) where f::AC)
            @test_skip replace(@term(f(x, y, z) where f::AC), Dict(@term(f(x, z) where f::AC) => :w)) == @term(f(w, y) where f::AC)
        end

    end

end
