using Rewrite: PatternRule, EvalRule, DivergentError
using SpecialSets


@testset "Rule" begin
    @testset "PatternRule" begin
        @test normalize(@term(x + 0 + 0), PatternRule{Term}(@term(a + 0), @term(a))) == @term(x + 0)
        @test normalize(@term(y + 1), PatternRule{Term}(@term(a + 0), @term(a))) == @term(y + 1)
        @test normalize(@term(y), PatternRule{Term}(@term(a + 0), @term(a))) == @term(y)
        @test normalize(@term(f(a, b)), TRS(@term(f(x, y)) => @term(g(x)))) == @term(g(a))
        with_context(AlgebraContext(props=Dict(:f => [Orderless]))) do
            @test_throws DivergentError normalize(@term(f(a, b)), TRS(@term(f(x, y)) => @term(g(x))))
        end
        @test normalize(@term(x + 0 + 0), TRS(@term(a + 0) => @term(a))) == @term(x)

        @testset "Predicates" begin
            nz = Variable(:nz, Nonzero)
            odd = Variable(:odd, Odd)

            @test normalize(@term(3 / 3), TRS(@term($nz / $nz) => @term(one($nz)))) == @term(one(3))
            @test normalize(@term(2 / 3), TRS(@term($nz / $nz) => @term(one($nz)))) == @term(2 / 3)
            @test normalize(@term(x / x), TRS(@term($nz / $nz) => @term(one($nz)))) == @term(x / x)
            @test normalize(@term((2^x) / (2^x)), TRS(@term($nz / $nz) => @term(one($nz)))) == @term(one(2^x))
            @test normalize(@term($odd / $odd), TRS(@term($nz / $nz) => @term(one($nz)))) == @term(one($odd))
            @test_skip normalize(@term(($odd + 2) / ($odd + 2)), TRS(@term($nz / $nz) => @term(one($nz)))) == @term(one($odd + 2))
        end
    end
    @testset "EvalRule" begin
        @test normalize(@term(f(2, 3)), EvalRule(:f, *)) == @term(6)
        @test normalize(@term(f(x, 3)), EvalRule(:f, *)) == @term(f(x, 3))
        @test normalize(@term(f(2, y)), EvalRule(:f, *)) == @term(f(2, y))
        @test normalize(@term("a" * "b"), EvalRule(*)) == @term("ab")
        @test normalize(@term(x + 2 * 3), EvalRule(*)) == @term(x + 2 * 3)
        @test normalize(@term(x + 2 * 3), TRS(EvalRule(*))) == @term(x + 6)
        @test normalize(@term(2 * 3 + 4 * 5), TRS(EvalRule(*))) == @term(6 + 20)
        @test normalize(@term(2 * 3 + 4 * 5), TRS(EvalRule(+), EvalRule(*))) == @term(26)

        with_context(AlgebraContext(props=Dict(:f => [Flat]))) do
            rule = EvalRule(:f, +)
            @test normalize(@term(f(a, 1, 2, b, 3, c)), rule) == @term(f(a, 3, b, 3, c))
            @test normalize(@term(f(1, 2, 3, 4, 5)), rule) == @term(15)
            @test normalize(@term(f(1, 2, x, 3, 4, 5)), rule) == @term(f(3, x, 12))
            @test normalize(@term(f(1, 2, x, y, 3, 4, 5)), rule) == @term(f(3, x, y, 12))
        end

        with_context(AlgebraContext(props=Dict(:f => [Flat, Orderless]))) do
            rule = EvalRule(:f, +)
            @test normalize(@term(f(a, 1, 2, b, 3, c)), rule) == @term(f(6, a, b, c))
            @test normalize(@term(f(1, 2, 3, 4, 5)), rule) == @term(15)
            @test normalize(@term(f(1, 2, x, 3, 4, 5)), rule) == @term(f(15, x))
            @test normalize(@term(f(1, 2, x, y, 3, 4, 5)), rule) == @term(f(15, x, y))
        end

        with_context(AlgebraContext(props=Dict(:f => [Flat]), orderless=Dict(:f => Set([2])))) do
            rule = EvalRule(:f, +)
            @test normalize(@term(f(2, x, y, 1, 3)), rule) == @term(f(x, y, 6))
            @test_throws DivergentError normalize(@term(f(1, x, y, 2, 3)), rule)
            @test normalize(@term(f(x, y, 1, 3)), rule) == @term(f(x, y, 4))
            @test normalize(@term(f(1, x, y, 3)), rule) == @term(f(1, x, y, 3))
        end
    end
end


@testset "accuracy of standard rules" begin
    @testset "$set" for set ∈ [:BASIC, :ABSOLUTE_VALUE, :BOOLEAN, :CALCULUS, :LOGARITHM, :TRIGONOMETRY, :TYPES]
        CASES = [-10:10;]

        set ∈ [:CALCULUS, :LOGARITHM] && continue

        for rule ∈ rules(set)
            rule isa PatternRule || continue
            l, r = rule

            @testset "$rule" begin
                for case ∈ CASES
                    vars = Rewrite.vars(l)
                    all(vars) do var
                        Set([case]) ⊆ Rewrite.image(var)
                    end || continue

                    args = [:($var = $case) for var ∈ parse.(vars)]

                    lres = Expr(:let, Expr(:block, args...), parse(l)) |> eval
                    rres = Expr(:let, Expr(:block, args...), parse(r)) |> eval

                    success = isapprox(lres, rres, atol = 1e-12)
                    success || @error "Case" case
                    @test success
                end
            end
        end
    end
end


@testset "normalize" begin

    @testset "STANDARD" begin
        x = Variable(:x, TypeSet(Number))
        y = Variable(:y, TypeSet(Number))

        @test normalize(@term(a)) == @term(a)
        @test normalize(@term($x)) == @term($x)
        @test normalize(@term($x + 0)) == @term($x)
        @test normalize(@term($y + 0 + 0)) == @term($y)
        @test normalize(@term($y * (1 + 2 - 3))) == @term(0)
        @test normalize(@term(0 + $y + 0)) == @term($y)
        @test normalize(@term($x^0.5 * $x^0.5)) == @term($x)
    end

    @testset "ABSOLUTE_VALUE" begin
        y = Variable(:y, Nonzero)

        @test normalize(@term(abs(x))) == @term(abs(x))
        @test normalize(@term(abs(-x))) == @term(abs(x))
        @test normalize(@term(abs(0))) == @term(0)
        @test normalize(@term(abs(-3))) == @term(3)
        @test normalize(@term(abs(3))) == @term(3)
        @test normalize(@term(abs(2x))) == @term(2abs(x))
        @test normalize(@term(abs(-(5x)))) == @term(5abs(x))
        @test normalize(@term(abs(x * y))) == @term(abs(x) * abs(y))
        @test normalize(@term(abs(x / $y))) == @term(abs(x) * inv(abs($y)))
        @test normalize(@term(abs(x / 1))) == @term(abs(x))
        @test normalize(@term(abs(abs(x)))) == @term(abs(x))
        @test normalize(@term(abs(x^2))) == @term(x^2)

        d1, d2 = Variable.([:d1, :d2], [Set([1,2]), Set([-1,1])])
        @test normalize(@term(abs($d1))) == @term($d1)
        @test normalize(@term(abs($d2))) == @term(abs($d2))
    end

    @testset "BOOLEAN" begin
        x, y = Variable.([:x, :y], Ref(TypeSet(Bool)))

        @test normalize(@term($x & true)) == @term($x)
        @test normalize(@term($x | ($x & $y))) == @term($x)
        @test normalize(@term($y | !$y)) == @term(true)
        @test normalize(@term(!$y | $y)) == @term(true)
        @test normalize(@term($y & $y)) == @term($y)
        @test normalize(@term(!(!$x))) == @term($x)
        @test normalize(@term(!(!$x & !$x))) == @term($x)
        @test normalize(@term(!(!$x & !$x) & !$x)) == @term(false)
        @test normalize(@term(!(!$x & !$x) | $x)) == @term($x)
        @test normalize(@term(!$x & $x | ($y & ($y | true)))) == @term($y)
    end

    @testset "CALCULUS" begin
        x, y, z = Variable.([:x, :y, :z], Ref(TypeSet(Int)))
        @test normalize(@term diff($x * $y, $x)) == @term($y)
        @test normalize(@term diff(sin(2*$x + 3*$y), $x)) == @term(2cos(2*$x + 3*$y))
        @test normalize(@term diff($x * $y + sin($x^$z), $x)) == @term($y + $x^($z + -1)*cos($x^$z)*$z)
        @test normalize(@term diff(2*$x + tan($x), $x)) == @term(3 + tan($x)^2)
        @test normalize(@term diff(f($x) + sin($x^2), $x)) == @term(2*$x*cos($x^2) + diff(f($x), $x))

        w = Variable(:w, Nonzero ∩ TypeSet(Float64))
        @test normalize(@term diff(log($w), $w)) == @term(inv($w))
    end

    @testset "LOGARITHM" begin
        n = Variable(:n, GreaterThan(3))

        @test normalize(@term(log(b, x * y))) == @term(log(b, x) + log(b, y))
        @test normalize(@term(log($n, 1))) == @term(0)
        @test normalize(@term(log($n, $n ^ x))) == @term(x)
        @test normalize(@term(log($n, $n * y))) == @term(1 + log($n, y))
        @test normalize(@term(log(b, 1/$n))) == @term(-log(b, $n))
        @test_skip normalize(@term(b ^ log(b, x*b))) == @term(x * b)
        @test_skip normalize(@term(b ^ (log(b, x) + log(b, b)))) == @term(x * b)
    end

    @testset "TRIGONOMETRY" begin
        @test normalize(@term(sin(0) * tan(π / 4))) == @term(0)
        @test normalize(@term(sin(π/3)cos(0) + cos(π/3)sin(0))) == @term(√3 * inv(2))
        @test normalize(@term(one(θ) + tan(θ) ^ 2)) == @term(sec(θ) ^ 2)
        @test normalize(@term(tan(π / 6))) == @term(√3 * inv(3))
        @test_broken normalize(@term(1 / (sin(-3) / cos(-3)))) == @term(-cot(3))
        @test normalize(@term(2 * cos((α + β) / 2) * cos((α - β) / 2))) == @term(cos(α) + cos(β))
        @test normalize(@term((tan(α) - tan(β)) * inv(1 + tan(α) * tan(β)))) == @term(tan(α + -β))
        @test normalize(@term(csc(π/2 - θ))) == @term(sec(θ))

        x = Variable(:x, TypeSet(Int))
        @test_broken normalize(@term sin($x)^2 + cos($x)^2 + 1) == 2
    end


    @testset "custom" begin
        @test normalize(@term(f(y, y)), @term RULES [
            f(x, x) => x
        ]) == @term y
        @test normalize(@term(f(x, y)), @term RULES [
            f(x, x) => x
        ]) == @term f(x, y)
        @test normalize(@term(f(f(x), x)), @term RULES [
            f(x, x) => x, f(x) => x
        ]) == @term x
        @test normalize(@term(f(f(x), g(x))), @term RULES [
            f(f(x), g(x)) => x, g(x) => x
        ]) == @term f(f(x), x)
    end
end
