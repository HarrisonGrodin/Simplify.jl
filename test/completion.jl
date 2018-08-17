using Rewrite.Completion: unify, Unifier, critical_pairs
using Rewrite.Completion: add_rule!, size, orient!, choose!, complete
using Rewrite.Completion: LPO
using Rewrite: PatternRule

@testset "Completion" begin

    @testset "unify" begin
        x, y, z = Variable.([:x, :y, :z])
        _1, _2 = Constant(1), Constant(2)
        f(xs...) = Fn(:f, xs...)
        g(xs...) = Fn(:g, xs...)

        @test unify(x, x) == Unifier()
        @test unify(x, y) == Unifier(x => y)
        @test unify(y, x) == Unifier(y => x)

        @test unify(_2, _2) == Unifier()
        @test unify(Constant(2), Constant(3)) === nothing
        @test unify(Constant(1), Constant("one")) === nothing

        @test unify(x, f(y)) == Unifier(x => f(y))
        @test unify(f(y), x) == Unifier(x => f(y))
        @test unify(x, f(x)) == nothing
        @test unify(f(x), f(y)) == Unifier(x => y)
        @test unify(f(x), g(y)) == nothing
        @test unify(g(x, x), g(y, z)) == Unifier(x => z, y => z)
        @test unify(g(f(x), x), g(f(y), z)) == Unifier(x => z, y => z)
        @test unify(g(f(x), x), g(y, y)) == nothing
        @test unify(g(f(x), x), g(y, z)) == Unifier(y => f(z), x => z)
        @test unify(f(x), f(x, y)) == nothing
    end

    @testset "critical_pairs" begin
        @test critical_pairs(
            PatternRule{Term}(@term(f(f(x, y), z)), @term(f(x, f(y, z)))),
            PatternRule{Term}(@term(f(f(x, y), z)), @term(f(x, f(y, z)))),
        ) == [
            (@term(f(x, f(y, z))), @term(f(x, f(y, z)))),
            (@term(f(f(x, y), f(z, z₁))), @term(f(f(x, f(y, z)), z₁))),
        ]

        @test critical_pairs(
            PatternRule{Term}(@term(f(f(x, y), z)), @term(f(x, f(y, z)))),
            PatternRule{Term}(@term(f(g(x), x)), @term(h())),
        ) == [
            (@term(f(g(x), f(x, z₁))), @term(f(h(), z₁)))
        ]

        @test critical_pairs(
            PatternRule{Term}(@term(f(x, g(x))), @term(h())),
            PatternRule{Term}(@term(f(h(), x)), @term(x)),
        ) == [
            (@term(h()), @term(g(h())))
        ]
    end

    @testset "add_rule!" begin
        es, ss, rs = [], TRS(), TRS()
        @test add_rule!(PatternRule{Term}(@term(f()), @term(g())), es, ss, rs) ==
            ([], TRS(@term(f()) => @term(g())), TRS())
        @test isempty(es)
        @test !isempty(ss) && ss == TRS(@term(f()) => @term(g()))
        @test isempty(rs)

        @test add_rule!(PatternRule{Term}(@term(f(x, y)), @term(g(x, y))), [], [PatternRule{Term}(@term(f(x, x)), @term(x))], []) ==
            ([(@term(g(x, x)), @term(x))], [PatternRule{Term}(@term(f(x, y)), @term(g(x, y)))], [])
        @test add_rule!(PatternRule{Term}(@term(f(x, y)), @term(g(x, y))), [], [], Pair[@term(f(x, x)) => @term(x)]) ==
            ([(@term(g(x, x)), @term(x))], [PatternRule{Term}(@term(f(x, y)), @term(g(x, y)))], [])
    end

    @testset "orient!" begin
        a >ᵣ b = size(a) > size(b)

        @test orient!(>ᵣ, [(@term(f(x)), @term(f(x)))], TRS(), TRS(@term(g(x)) => @term(x))) ==
            (TRS(), TRS(@term(g(x)) => @term(x)))
        @test orient!(>ᵣ, [(@term(f(x)), @term(x))], TRS(), TRS(@term(f(x)) => @term(x))) ==
            (TRS(), TRS(@term(f(x)) => @term(x)))
        @test orient!(>ᵣ, [(@term(f(x, x)), @term(x))], TRS(), TRS(@term(f(x, y)) => @term(g(x)))) ==
            (TRS(@term(g(x)) => @term(x)), TRS(@term(f(x, y)) => @term(x)))
        @test orient!(>ᵣ, [(@term(x), @term(f(x, x)))], TRS(), TRS(@term(f(x, y)) => @term(g(x)))) ==
            (TRS(@term(g(x)) => @term(x)), TRS(@term(f(x, y)) => @term(x)))
        @test (@test_logs (:warn, "Unable to determine preferred form") orient!(
            >ᵣ, [(@term(f(x, x)), @term(h(x, x)))], TRS(), TRS()
        )) === nothing
    end

    @testset "choose!" begin
        rs = TRS(@term(f(f(f(x)))) => @term(x), @term(g(x)) => @term(x))
        @test choose!(rs) == PatternRule{Term}(@term(g(x)), @term(x))
        @test rs == TRS(@term(f(f(f(x)))) => @term(x))

        rs = TRS(@term(f(x)) => @term(x), @term(g(x)) => @term(x))
        @test choose!(rs) == PatternRule{Term}(@term(f(x)), @term(x))

        rs = TRS(@term(g(x)) => @term(x), @term(f(x)) => @term(x))
        @test choose!(rs) == PatternRule{Term}(@term(g(x)), @term(x))

        @test_throws ArgumentError choose!([])
    end

    @testset "complete" begin
        >ᵣ = LPO((:i, 1), (:*, 2), (:e, 0))
        with_context(AlgebraContext()) do
            @test Set(complete(>ᵣ, @term AXIOMS [
                ((x * y) * z  , x * (y * z)  )
                (i(x) * x     , e()          )
                (e() * x      , x            )
            ])) == Set(@term RULES [
                    (x * y) * z  =>  x * (y * z)
                        e() * x  =>  x
                        x * e()  =>  x
                       x * i(x)  =>  e()
                       i(x) * x  =>  e()
                         i(e())  =>  e()
                        i(i(x))  =>  x
                     i(x₁ * y₁)  =>  i(y₁) * i(x₁)
                x * (i(x) * z₃)  =>  z₃
                i(x) * (x * z₁)  =>  z₁
            ])
        end
    end

    @testset "lpo" begin
        >ₗₚₒ = LPO((:i, 1), (:f, 2), (:e, 0))
        @test !(@term(x) >ₗₚₒ @term(y))
        @test @term(f(x, e())) >ₗₚₒ @term(x)
        @test @term(i(e())) >ₗₚₒ @term(e())
        @test @term(i(f(x, y))) >ₗₚₒ @term(f(i(y), i(x)))
        @test @term(f(f(x, y), z)) >ₗₚₒ @term(f(x, f(y, z)))
        @test @term(f(x, y)) >ₗₚₒ @term(x)

        @test_throws ArgumentError("(:f, 1) is not contained in order") LPO((:i, 1))(@term(f(x)), @term(i(x)))
        @test_throws ArgumentError("(:g, 1) is not contained in order") LPO((:i, 1))(@term(i(x)), @term(g(x)))
    end

end
