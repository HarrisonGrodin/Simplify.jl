using SymReduce.Completion: normalize, critical_pairs, add_rule!, size, orient!, choose!, complete, LPO

@testset "Completion" begin

    @testset "critical_pairs" begin
        f(xs...) = Fn{:f}(xs...)
        g(xs...) = Fn{:g}(xs...)
        h(xs...) = Fn{:h}(xs...)
        x, y, z = Variable.([:x, :y, :z])
        x1, y1, z1 = Variable.([:x, :y, :z], 1)

        @test critical_pairs(
            f(f(x, y), z) => f(x, f(y, z)),
            f(f(x, y), z) => f(x, f(y, z)),
        ) == [
            (f(x, f(y, z)), f(x, f(y, z))),
            (f(f(x, y), f(z, z1)), f(f(x, f(y, z)), z1)),
        ]

        @test critical_pairs(
            f(f(x, y), z) => f(x, f(y, z)),
            f(g(x), x) => h(),
        ) == [
            (f(g(x), f(x, z1)), f(h(), z1))
        ]

        @test critical_pairs(
            f(x, g(x)) => h(),
            f(h(), x) => x,
        ) == [
            (h(), g(h()))
        ]
    end

    @testset "add_rule!" begin
    f(xs...) = Fn{:f}(xs...)
    g(xs...) = Fn{:g}(xs...)
    h(xs...) = Fn{:h}(xs...)
        x, y, z = Variable.([:x, :y, :z])
        x1, y1, z1 = Variable.([:x, :y, :z], 1)

        es, ss, rs = [], [], []
        @test add_rule!(f() => g(), es, ss, rs) == ([], [f() => g()], [])
        @test isempty(es)
        @test !isempty(ss) && ss == [f() => g()]
        @test isempty(rs)

        @test add_rule!(f(x, y) => g(x, y), [], Pair[f(x, x) => x], []) ==
            ([(g(x, x), x)], [f(x, y) => g(x, y)], [])
        @test add_rule!(f(x, y) => g(x, y), [], [], Pair[f(x, x) => x]) ==
            ([(g(x, x), x)], [f(x, y) => g(x, y)], [])
    end

    @testset "orient!" begin
        a >ᵣ b = size(a) > size(b)

        f(xs...) = Fn{:f}(xs...)
        g(xs...) = Fn{:g}(xs...)
        h(xs...) = Fn{:h}(xs...)
        x, y, z = Variable.([:x, :y, :z])
        x1, y1, z1 = Variable.([:x, :y, :z], 1)

        @test orient!(>ᵣ, [(f(x), f(x))], [], Pair[g(x) => x]) == ([], [g(x) => x])
        @test orient!(>ᵣ, [(f(x), x)], [], Pair[f(x) => x]) == ([], [f(x) => x])
        @test orient!(>ᵣ, [(f(x, x), x)], [], Pair[f(x, y) => g(x)]) ==
            ([g(x) => x], [f(x, y) => x])
        @test orient!(>ᵣ, [(x, f(x, x))], [], Pair[f(x, y) => g(x)]) ==
            ([g(x) => x], [f(x, y) => x])
        @test (@test_logs (:warn, "Unable to determine preferred form") orient!(>ᵣ, [(f(x, x), h(x, x))], [], [])) === nothing
    end

    @testset "choose!" begin
        f(xs...) = Fn{:f}(xs...)
        g(xs...) = Fn{:g}(xs...)
        x, y, z = Variable.([:x, :y, :z])

        rs = [f(f(f(x))) => x, g(x) => x]
        @test choose!(rs) == (g(x) => x)
        @test rs == [f(f(f(x))) => x]

        rs = [f(x) => x, g(x) => x]
        @test choose!(rs) == (f(x) => x)

        rs = [g(x) => x, f(x) => x]
        @test choose!(rs) == (g(x) => x)

        @test_throws ArgumentError choose!([])
    end

    @testset "complete" begin
        >ᵣ = LPO(Fn{:i,1}, Fn{:*,2}, Fn{:e,0})
        @test Set(complete(>ᵣ, @term [
            ((x * y) * z  , x * (y * z)  ),
            (i(x) * x     , e()          ),
            (e() * x      , x            ),
        ])) == Set(@term PAIRS [
                (x * y) * z  =>  x * (y * z),
                    e() * x  =>  x,
                    x * e()  =>  x,
                   x * i(x)  =>  e(),
                   i(x) * x  =>  e(),
                     i(e())  =>  e(),
                    i(i(x))  =>  x,
                 i(x₁ * y₁)  =>  i(y₁) * i(x₁),
            x * (i(x) * z₃)  =>  z₃,
            i(x) * (x * z₁)  =>  z₁,
        ])
    end

    @testset "lpo" begin
        >ₗₚₒ = LPO(Fn{:i,1}, Fn{:f,2}, Fn{:e,0})
        @test !(@term(x) >ₗₚₒ @term(y))
        @test @term(f(x, e())) >ₗₚₒ @term(x)
        @test @term(i(e())) >ₗₚₒ @term(e())
        @test @term(i(f(x, y))) >ₗₚₒ @term(f(i(y), i(x)))
        @test @term(f(f(x, y), z)) >ₗₚₒ @term(f(x, f(y, z)))
        @test @term(f(x, y)) >ₗₚₒ @term(x)
    end

end
