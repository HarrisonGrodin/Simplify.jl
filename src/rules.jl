export rules

import SymReduce.Patterns: @term


abstract type Rule end

struct PatternRule <: Rule
    left::Term
    right::Term
end
function Base.iterate(r::PatternRule, state=:left)
    state === :left  && return (r.left, :right)
    state === :right && return (r.right, nothing)
    nothing
end
function normalize(t::Term, (l, r)::PatternRule)
    σ = match(l, t)
    σ === nothing && return t
    σ(r)
end

struct EvalRule{F,N} <: Rule
    f
end
EvalRule(f, N) = EvalRule{nameof(f),N}(f)
function normalize(t::Fn{F,N}, r::EvalRule{F,N}) where {F,N}
    all(arg -> arg isa Constant, t) || return t
    args = get.(collect(t))
    Constant(r.f(args...))
end
normalize(t::Term, ::EvalRule) = t


macro term(::Val{:RULES}, ex)
    @assert ex.head == :vect
    args = map(ex.args) do pair
        p, a, b = pair.args
        @assert p == :(=>)
        :(PatternRule($(parse(Term, a)), $(parse(Term, b))))
    end
    :(PatternRule[$(args...)])
end
rules(set::Symbol=:STANDARD, args...; kwargs...) = rules(Val(set), args...; kwargs...)


rules(::Val{:STANDARD}) = [@term RULES [
    x + 0      => x,
    0 + x      => x,
    x * 1      => x,
    1 * x      => x,
    x * 0      => 0,
    0 * x      => 0,
    x + -y     => x - y,
    x - x      => 0,
    x * inv(y) => x / y,
]; [
    EvalRule(+, 2),
    EvalRule(-, 1),
    EvalRule(-, 2),
    EvalRule(*, 2),
]; rules.([:BOOLEAN, :TRIGONOMETRY])...]


rules(::Val{:BOOLEAN}; and=:&, or=:|, neg=:!) = [@term RULES [
    $or(x, false) => x,
    $and(x, true) => x,

    $or(x, true) => true,
    $and(x, false) => false,

    $or(x, x) => x,
    $and(x, x) => x,

    $or(x, $and(x, y)) => x,
    $and(x, $or(x, y)) => x,

    $or(x, $neg(x)) => true,
    $and(x, $neg(x)) => false,

    $neg($neg(x)) => x,
]; [
    EvalRule{and,2}(&),
    EvalRule{or,2}(|),
    EvalRule{neg,1}(!),
]]


rules(::Val{:TRIGONOMETRY}) = @term RULES [
    # Common angles
    sin(0) => 0,
    cos(0) => 1,
    tan(0) => 0,

    sin(π / 6) => 1 / 2,
    cos(π / 6) => √3 / 2,
    tan(π / 6) => √3 / 3,

    sin(π / 4) => √2 / 2,
    cos(π / 4) => √2 / 2,
    tan(π / 4) => 1,

    sin(π / 3) => √3 / 2,
    cos(π / 3) => 1 / 2,
    tan(π / 3) => √3,

    sin(π / 2) => 1,
    cos(π / 2) => 0,
    # tan(π / 2) => # TODO: infinite/undefined


    # Definitions of relations
    sin(θ) / cos(θ) => tan(θ),
    cos(θ) / sin(θ) => cot(θ),
    1 / cos(θ) => sec(θ),
    1 / sec(θ) => cos(θ),
    1 / sin(θ) => csc(θ),
    1 / csc(θ) => sin(θ),
    1 / tan(θ) => cot(θ),
    1 / cot(θ) => tan(θ),

    # Pythagorean identities
    sin(θ)^2 + cos(θ)^2 => one(θ),
    one(θ) + tan(θ)^2 => sec(θ)^2,  # NOTE: will not match any one constants
    one(θ) + cot(θ^2) => csc(θ)^2,

    # Negative angles
    sin(-θ) => -sin(θ),
    cos(-θ) => cos(θ),
    tan(-θ) => tan(θ),

    # Angle sum and difference identities
    sin(α)cos(β) + cos(α)sin(β) => sin(α + β),
    sin(α)cos(β) - cos(α)sin(β) => sin(α - β),
    cos(α)cos(β) - sin(α)sin(β) => cos(α + β),
    cos(α)cos(β) + sin(α)sin(β) => cos(α - β),

    # Double-angle formulae
    2sin(θ)cos(θ) => sin(2θ),
    cos(θ)^2 - sin(θ)^2 => cos(2θ),
    2cos(θ)^2 - 1 => cos(2θ),
]
