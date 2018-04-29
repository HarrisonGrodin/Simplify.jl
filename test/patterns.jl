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
end
