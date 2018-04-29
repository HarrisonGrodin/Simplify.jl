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
    @testset "TypeSet" begin
        ints = TypeSet{Int}()
        integers = TypeSet{Integer}()
        strings = TypeSet{String}()

        @test ints == ints
        @test ints ≠ integers

        @test match(integers, ints) == Dict()
        @test ints ⊆ integers
        @test match(ints, integers) === nothing
        @test integers ⊈ ints
        @test match(ints, strings) === nothing
        @test strings ⊈ ints

        @test replace(ints, Dict(Variable(:x) => Variable(:y))) == ints
    end
    @testset "Constant" begin
        a, b = Constant{Int}(1), Constant{Integer}(1)

        @test a == a
        @test a ≠ b

        @test match(b, a) == Dict()
        @test a ⊆ b
        @test match(a, b) === nothing
        @test b ⊈ a

        @test replace(a, Dict(Variable(:x) => Variable(:y))) == a
    end
end
