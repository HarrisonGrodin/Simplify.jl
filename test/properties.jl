using SpecialSets


@testset "Properties" begin

    @testset "Arity" begin
        @testset "self" begin
            @test isvalid(Arity(sin, 1), Context(Arity(sin, 1)))
            @test !isvalid(Arity(sin, 1), Context(Arity(cos, 1)))
            @test !isvalid(Arity(sin, 1), Context(Arity(sin, 2)))
        end
    end

    @testset "Image" begin
        @vars α
        @syms a b

        @testset "self" begin
            @test isvalid(Image(2, Set([2])), Context())
            @test !isvalid(Image(2, Set([1])), Context())
            @test isvalid(Image(2, Nonnegative), Context())
            @test !isvalid(Image(α, Nonnegative), Context())

            @test isvalid(Image(a, TypeSet(Int)), Context(
                Image(a, TypeSet(Int))
            ))
            @test isvalid(Image(a, TypeSet(Number)), Context(
                Image(a, TypeSet(Int))
            ))
            @test !isvalid(Image(a, TypeSet(Int)), Context(
                Image(a, TypeSet(Number))
            ))
            @test !isvalid(Image(b, TypeSet(Int)), Context(
                Image(a, TypeSet(Number))
            ))
        end
    end

    @testset "Signature" begin
        @syms a b

        @testset "self" begin
            @test isvalid(Signature(+, [Positive, Positive], Positive), Context(
                Signature(+, [Positive, Positive], Positive),
            ))
            @test !isvalid(Signature(+, [Positive, Positive], Positive), Context(
                Signature(*, [Positive, Positive], Positive),
            ))
            @test !isvalid(Signature(+, [Positive, Positive, Positive], Positive), Context(
                Signature(+, [Positive, Positive], Positive),
            ))
        end

        @testset "Arity" begin
            @test isvalid(Arity(+, 2), Context(
                Signature(+, [Positive, Positive], Positive),
            ))
            @test !isvalid(Arity(+, 3), Context(
                Signature(+, [Positive, Positive], Positive),
            ))
        end

        @testset "Image" begin
            @test isvalid(Image(get(@term 1 + 2), Positive), Context(
                Signature(+, [Positive, Positive], Positive),
            ))
            @test !isvalid(Image(get(@term 1 + 2), Positive), Context(
                Signature(+, [Positive], Positive),
            ))
            @test !isvalid(Image(get(@term 1 + b), Positive), Context(
                Signature(+, [Positive, Positive], Positive),
            ))
            @test isvalid(Image(get(@term 1 + b), Positive), Context(
                Signature(+, [Positive, Positive], Positive),
                Image(b, Positive),
            ))
        end
    end

    @testset "Closure" begin
        @syms a b

        @testset "self" begin
            @test isvalid(Closure(+, Nonnegative), Context(
                Closure(+, Nonnegative),
            ))
            @test !isvalid(Closure(+, Nonnegative), Context(
                Closure(*, Nonnegative),
            ))
        end

        @testset "Signature" begin
            @test isvalid(Signature(+, [Positive, Positive], Positive), Context(
                Closure(+, Positive),
            ))
            @test isvalid(Signature(+, [Positive, Positive], Positive), Context(
                Closure(+, Nonnegative),
            ))
            @test isvalid(Signature(+, [Positive, Positive, Positive], Positive), Context(
                Closure(+, Positive),
            ))
        end

        @testset "Image" begin
            @test isvalid(Image(get(@term 1 + 2), Positive), Context(
                Closure(+, Positive),
            ))
            @test !isvalid(Image(get(@term 1 + 2), Set([3])), Context(
                Closure(+, Positive),
            ))
            @test !isvalid(Image(get(@term 0 + 2), Positive), Context(
                Closure(+, Positive),
            ))
            @test isvalid(Image(get(@term 1 + (2 * 3)), Positive), Context(
                Closure(+, Positive),
                Closure(*, Positive),
            ))
        end
    end

    @testset "Associative" begin
        @testset "self" begin
            @test isvalid(Associative(+), Context(Associative(+)))
            @test !isvalid(Associative(+), Context(Associative(*)))
            @test isvalid(Associative(+), Context(Associative(+), Associative(*)))
        end

        @testset "Arity" begin
            @test isvalid(Arity(+, 2), Context(Associative(+)))
            @test isvalid(Arity(+, 3), Context(Associative(+)))
            @test !isvalid(Arity(+, 2), Context(Associative(*)))
        end
    end

    @testset "Commutative" begin
        @testset "self" begin
            @test isvalid(Commutative(+), Context(Commutative(+)))
            @test !isvalid(Commutative(+), Context(Commutative(*)))
        end
    end

end
