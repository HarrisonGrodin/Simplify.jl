using SymReduce.Patterns
using SymReduce.Patterns: Match, Unify

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

        @test unify(x, f(y)) == Unify(x => f(y))
        @test unify(f(y), x) == Unify(x => f(y))
        @test unify(x, f(x)) === nothing
        @test unify(f(x), f(y)) == Unify(x => y)
        @test unify(g(x, x), g(y, z)) == Unify(x => z, y => z)
        @test unify(g(f(x), x), g(f(y), z)) == Unify(x => z, y => z)
        @test unify(g(f(x), x), g(y, y)) === nothing
        @test unify(g(f(x), x), g(y, z)) == Unify(y => f(z), x => z)
        @test unify(f(x), f(x, y)) === nothing

        @test match(f(), f()) == Match()
        @test match(f(x), f(y)) == Match(x => y)
        @test f(y) ⊆  f(x)
        @test match(f(x), g(x)) === nothing
        @test match(f(f(), x), f(g(), y)) === nothing
        @test match(f(x, x), f(y, z)) === nothing
        @test match(f(x), g(x, y)) === nothing
        @test g(x, y) ⊈ f(x)
        @test @term(f(a, 2, b)) ⊈ @term(f(x, 2, x))

        @test replace(f(x), Dict(x => y)) == f(y)
        @test replace(f(x), Dict(y => x)) == f(x)
    end
    @testset "Constant" begin
        one, two = Constant(1), Constant(2)

        @test one == one
        @test one ≠ two

        @test one == @term 1

        @test unify(two, two) == Unify()
        @test unify(Constant(2), Constant(3)) === nothing
        @test unify(Constant(1), Constant("one")) === nothing

        @test match(one, one) == Match()
        @test one ⊆ one
        @test match(two, one) === nothing
        @test one ⊈ two
        @test match(@term(x + 0), @term(y + 0)) == Match(@term(x) => @term(y))

        @test replace(one, Dict(@term(x) => @term(y))) == one
    end
end
