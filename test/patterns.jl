using SymReduce.Patterns

@testset "Patterns" begin
    @testset "Variable" begin
        a, b, x, y = Variable.([:a, :b, :x, :y])

        @test x == x
        @test x ≠ y

        @test_skip match(a, b) == Dict(a => b)
        @test_skip b ⊆ a

        @test_skip replace(a, Dict(a => b)) == b
        @test_skip replace(a, Dict(x => b)) == a
    end
    @testset "Function" begin
        x, y = Variable.([:x, :y])
        f1 = Fn(:f, x)
        f2 = Fn(:f, y)
        g = Fn(:g, x, y)

        @test f1 == f1
        @test f1 == Fn(:f, x)
        @test f1 ≠ f2

        @test_skip match(f1, f2) == Dict(x => y)
        @test_skip f2 ⊆ f1
        @test_skip match(f1, g) === nothing
        @test_skip g ⊈ f1

        @test_skip replace(f1, Dict(x => y)) == Fn{:f}(y)
        @test_skip replace(f1, Dict(y => x)) == f1
    end
    @testset "TypeSet" begin
        ints = TypeSet{Int}()
        integers = TypeSet{Integer}()
        strings = TypeSet{String}()

        @test ints == ints
        @test ints ≠ integers

        @test_skip match(integers, ints) == Dict()
        @test_skip ints ⊆ integers
        @test_skip match(ints, integers) === nothing
        @test_skip integers ⊈ ints
        @test_skip match(ints, strings) === nothing
        @test_skip strings ⊈ ints

        @test_skip replace(ints, Dict(Variable(:x) => Variable(:y))) == ints
    end
    @testset "Constant" begin
        a, b = Constant{Int}(1), Constant{Integer}(1)

        @test a == a
        @test a ≠ b

        @test_skip match(b, a) == Dict()
        @test_skip a ⊆ b
        @test_skip match(a, b) === nothing
        @test_skip b ⊈ a

        @test_skip replace(a, Dict(Variable(:x) => Variable(:y))) == a
    end
end
