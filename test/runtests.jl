using SymReduce
using Test

include("patterns.jl")

@testset "normalize" begin
    @test normalize(@term(x)) == @term(x)
    @test normalize(@term(x + 0)) == @term(x)
    @test normalize(@term((y + 0) + 0)) == @term(y)
    @test_skip normalize(@term(y + 0 + 0)) == @term(y)
    @test normalize(@term(0 + (y + 0))) == @term(y)
end
