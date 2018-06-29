using SymReduce.Patterns

@testset "Patterns" begin
    @testset "Variable" begin
        a, b, x, y = Variable.([:a, :b, :x, :y])

        @test x == x
        @test x ≠ y
        @test a == @term a

        @test unify(x, x) == Substitution()
        @test unify(x, y) == Substitution(x => y)
        @test unify(y, x) == Substitution(y => x)

        @test match(a, b) == Substitution(a => b)
        @test b ⊆ a

        @test replace(a, Substitution(a => b)) == b
        @test replace(a, Substitution(x => b)) == a
    end
    @testset "Function" begin
        x, y, z = Variable.([:x, :y, :z])
        f(xs...) = Fn{:f}(xs...)
        g(xs...) = Fn{:g}(xs...)

        @test f(x) == f(x)
        @test f(x) ≠ f(y)
        @test f(x) == @term f(x)

        @test @term(f(x, y))[1] == @term(x)
        @test_throws BoundsError @term(f(x, y))[3]
        @test @term(f(x, g(h(y), f(z))))[2, 1] == @term(h(y))
        @test @term(f(x, g(h(y), f(z))))[2, 1, 1] == @term(y)
        @test_throws MethodError @term(f(x))[1, 1]

        @test unify(x, f(y)) == Substitution(x => f(y))
        @test unify(f(y), x) == Substitution(x => f(y))
        @test unify(x, f(x)) === nothing
        @test unify(f(x), f(y)) == Substitution(x => y)
        @test unify(g(x, x), g(y, z)) == Substitution(x => z, y => z)
        @test unify(g(f(x), x), g(f(y), z)) == Substitution(x => z, y => z)
        @test unify(g(f(x), x), g(y, y)) === nothing
        @test unify(g(f(x), x), g(y, z)) == Substitution(y => f(z), x => z)
        @test unify(f(x), f(x, y)) === nothing

        @test match(f(x), f(y)) == Substitution(x => y)
        @test f(y) ⊆  f(x)
        @test match(f(x, x), f(y, z)) === nothing
        @test match(f(x), g(x, y)) === nothing
        @test g(x, y) ⊈ f(x)

        @test replace(f(x), Substitution(x => y)) == f(y)
        @test replace(f(x), Substitution(y => x)) == f(x)
    end
    @testset "Constant" begin
        one, two = Constant(1), Constant(2)

        @test one == one
        @test one ≠ two

        @test one == @term 1

        @test unify(two, two) == Substitution()
        @test unify(Constant(2), Constant(3)) === nothing
        @test unify(Constant(1), Constant("one")) === nothing

        @test match(one, one) == Substitution()
        @test one ⊆ one
        @test match(two, one) === nothing
        @test one ⊈ two
        @test match(@term(x + 0), @term(y + 0)) == Substitution(@term(x) => @term(y))

        @test replace(one, Substitution(@term(x) => @term(y))) == one
    end
end
