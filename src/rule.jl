using DiffRules

export Rules
export normalize


abstract type AbstractRule end


struct Rules
    rules::Vector{AbstractRule}
    Rules(rs::Vector{AbstractRule}) = new(rs)
end
Rules(rs...) = Rules(collect(AbstractRule, rs))
Base.iterate(rs::Rules) = iterate(rs.rules)
Base.iterate(rs::Rules, state) = iterate(rs.rules, state)
Base.push!(rs::Rules, rule) = (push!(rs.rules, rule); rs)
Base.vcat(rss::Rules...) = Rules([(rs.rules for rs ∈ rss)...;])


normalize(rs::Rules) = Base.Fix2(normalize, rs)
function normalize(t::Term, rs::Rules)
    while true
        t = map(normalize(rs), t)
        t′ = foldl(normalize, rs; init=t)
        t == t′ && return t
        t = t′
    end
end
normalize(::T, ::R) where {T,R<:AbstractRule} = error("normalize undefined for rule type $R on term type $T")
normalize(t::Term) = normalize(t, rules())


struct PatternRule <: AbstractRule
    left::Term
    right::Term
    ps::Vector{Function}
end
PatternRule(l, r) = PatternRule(l, r, [])
PatternRule((l, r)::Pair) = PatternRule(l, r)
Base.convert(::Type{PatternRule}, p::Pair) = PatternRule(p)
Base.convert(::Type{AbstractRule}, p::Pair) = convert(PatternRule, p)
function normalize(t::Term, r::PatternRule)
    σ = match(r.left, t)
    σ === nothing && return t
    all(p -> p(σ), r.ps) || return t

    return replace(r.right, σ)
end

struct EvalRule <: AbstractRule
    name
    f
end
EvalRule(f::Function) = EvalRule(f, f)
normalize(t::Term, r::EvalRule) = Term(normalize(get(t), r))
function normalize(ex::Expr, r::EvalRule)
    ex.head === :call || return ex
    f, args = ex.args[1], ex.args[2:end]

    match(Term(r.name), Term(f)) === nothing && return ex

    if isvalid(Associative(f))
        if isvalid(Commutative(f))
            inds = findall(is_constant, args)
            if !isempty(inds)
                res = r.f(args[inds]...)
                deleteat!(args, inds)
                push!(args, res)
            end
        else
            _apply_associative!(r, args)
        end

        length(args) == 1 && return first(args)
        return Expr(:call, f, args...)
    end

    all(is_constant, args) || return ex
    r.f(args...)
end
normalize(x, ::EvalRule) = x
function _apply_associative!(r::EvalRule, args)
    i = firstindex(args)

    while i ≤ lastindex(args) - 1
        a, b = args[i:i+1]

        if is_constant(a) && is_constant(b)
            deleteat!(args, i)
            args[i] = r.f(a, b)
        else
            i += 1
        end
    end

    args
end


struct OrderRule <: AbstractRule
    by::Function
end
normalize(t::Term, r::OrderRule) = Term(normalize(get(t), r))
function normalize(ex::Expr, r::OrderRule)
    ex.head === :call || return ex
    f = ex.args[1]
    isvalid(Commutative(f)) || return ex
    issorted(ex.args[2:end], by = r.by) && return ex
    args = sort(ex.args[2:end], by = r.by)
    Expr(ex.head, f, args...)
end
normalize(x, ::OrderRule) = x
