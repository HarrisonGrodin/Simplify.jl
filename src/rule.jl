using DiffRules

export Rules
export normalize


abstract type Rule end


struct Rules
    rules::Vector{Rule}
    Rules(rs::Vector{Rule}) = new(rs)
end
Rules(rs...) = Rules(collect(Rule, rs))
Base.:(==)(a::Rules, b::Rules) = a.rules == b.rules
Base.iterate(rs::Rules) = iterate(rs.rules)
Base.iterate(rs::Rules, state) = iterate(rs.rules, state)
Base.length(rs::Rules) = length(rs.rules)
Base.getindex(rs::Rules, ind) = getindex(rs.rules, ind)
Base.push!(rs::Rules, rule) = (push!(rs.rules, rule); rs)
Base.pushfirst!(rs::Rules, rule) = (push!(rs.rules, rule); rs)
Base.pop!(rs::Rules) = pop!(rs.rules)
Base.popfirst!(rs::Rules) = popfirst!(rs.rules)
Base.deleteat!(rs::Rules, ind) = (deleteat!(rs.rules, ind); rs)
Base.vcat(rss::Rules...) = Rules([(rs.rules for rs ∈ rss)...;])


normalize(rs::Rules) = Base.Fix2(normalize, rs)
function normalize(t::Term, rs::Rules)
    while true
        t = map(normalize(rs), t)  # FIXME: replace with `subexpressions`
        t′ = foldl(normalize, rs; init=t)
        t == t′ && return t
        t = t′
    end
end
normalize(::T, ::R) where {T,R<:Rule} = error("normalize undefined for rule type $R on term type $T")
normalize(t::Term, sets::Symbol...) = normalize(t, vcat(rules.(sets)...))
normalize(t::Term) = normalize(t, rules())



struct DivergentError <: Exception
    forms::Vector{Term}
end
DivergentError(forms::Term...) = DivergentError(collect(forms))
function Base.showerror(io::IO, err::DivergentError)
    print(io, "DivergentError: ")
    join(io, err.forms, ", ")
end


struct PatternRule <: Rule
    left::Term
    right::Term
    ps::Vector{Function}
end
PatternRule(l, r) = PatternRule(l, r, [])
PatternRule((l, r)::Pair) = PatternRule(l, r)
Base.convert(::Type{PatternRule}, p::Pair) = PatternRule(p)
Base.convert(::Type{Rule}, p::Pair) = convert(PatternRule, p)
Base.convert(::Type{Pair}, r::PatternRule) = r.left => r.right
Base.:(==)(a::PatternRule, b::PatternRule) = (a.left, a.right) == (b.left, b.right)
Base.hash(p::PatternRule, h::UInt) = hash((p.left, p.right), hash(PatternRule, h))
Base.map(f, r::PatternRule) = PatternRule(f(r.left), f(r.right), r.ps)
function normalize(t::Term, r::PatternRule)
    Θ = match(r.left, t) |> collect
    isempty(Θ) && return t

    σᵢ = findfirst(_preds_match(r.ps), Θ)
    σᵢ === nothing && return t
    σ = Θ[σᵢ]

    replace(r.right, σ)
end
_preds_match(ps, σ) = all(p -> p(σ), ps)
_preds_match(ps) = Base.Fix1(_preds_match, ps)

struct EvalRule <: Rule
    name
    f
end
EvalRule(f::Function) = EvalRule(f, f)
normalize(t::Term, r::EvalRule) = Term(normalize(get(t), r))
function normalize(ex::Expr, r::EvalRule)
    ex.head === :call || return ex
    f, args = ex.args[1], ex.args[2:end]

    match(Term(r.name), Term(f)) == zero(Match) && return ex

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


struct OrderRule <: Rule
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


struct DiffRule <: Rule end
normalize(t::Term, r::DiffRule) = Term(normalize(get(t), r))
function normalize(ex::Expr, r::DiffRule)
    fn = ex.args[1]
    fn === diff && length(ex.args) == 3 || return ex
    f, x = ex.args[[2, 3]]
    is_ground(f) && return :($zero($f))
    ex
end
normalize(x, ::DiffRule) = x
