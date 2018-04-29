using SymReduce.Patterns

@testset "Patterns" begin
    @testset "Variable" begin
        a, b, x, y = Variable.([:a, :b, :x, :y])

        @test x == x
        @test x ≠ y
        @test_skip x ≠ Variable(:x)

        @test match(a, b) == Dict(a => b)
        @test b ⊆ a

        @test replace(a, Dict(a => b)) == b
        @test replace(a, Dict(x => b)) == a
    end
    @testset "Function" begin
        x, y = Variable.([:x, :y])
        f1 = Fn{:f,1}(x)
        f2 = Fn{:f}(y)
        g = Fn{:g}(x,y)

        @test f1 == f1
        @test f1 == Fn{:f}(x)
        @test f1 ≠ f2

        @test match(f1, f2) == Dict(x => y)
        @test f2 ⊆ f1
        @test match(f1, g) === nothing
        @test g ⊈ f1

        @test replace(f1, Dict(x => y)) == Fn{:f}(y)
        @test replace(f1, Dict(y => x)) == f1
    end
end
