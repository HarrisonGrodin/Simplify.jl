using DiffRules

export TermRewritingSystem, TRS
export normalize


abstract type Rule{T} end


struct AbstractRewritingSystem{T}
    rules::Vector{Rule{T}}
end
AbstractRewritingSystem{T}(rs::Union{Rule,Pair}...) where {T} =
    AbstractRewritingSystem{T}(collect(rs))
Base.:(==)(a::AbstractRewritingSystem, b::AbstractRewritingSystem) = a.rules == b.rules
Base.iterate(ars::AbstractRewritingSystem) = iterate(ars.rules)
Base.iterate(ars::AbstractRewritingSystem, state) = iterate(ars.rules, state)
Base.length(ars::AbstractRewritingSystem) = length(ars.rules)
Base.getindex(ars::AbstractRewritingSystem, ind) = getindex(ars.rules, ind)
Base.push!(ars::AbstractRewritingSystem, rule) = (push!(ars.rules, rule); ars)
Base.pushfirst!(ars::AbstractRewritingSystem, rule) = (push!(ars.rules, rule); ars)
Base.pop!(ars::AbstractRewritingSystem) = pop!(ars.rules)
Base.popfirst!(ars::AbstractRewritingSystem) = popfirst!(ars.rules)
Base.deleteat!(ars::AbstractRewritingSystem, ind) = (deleteat!(ars.rules, ind); ars)
Base.vcat(arss::AbstractRewritingSystem{T}...) where {T} = AbstractRewritingSystem([(ars.rules for ars ∈ arss)...;])

const TermRewritingSystem = AbstractRewritingSystem{Term}
const TRS = TermRewritingSystem


normalize(ars::AbstractRewritingSystem) = Base.Fix2(normalize, ars)
function normalize(t::Term, trs::TermRewritingSystem)
    while true
        t = map(normalize(trs), t)  # FIXME: replace with `subexpressions`
        t′ = foldl(normalize, trs; init=t)
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


struct PatternRule{T} <: Rule{T}
    left::T
    right::T
    ps::Vector{Function}
end
PatternRule{T}(l, r) where {T} = PatternRule{T}(l, r, [])
PatternRule{T}((l, r)::Pair{<:T,<:T}) where {T} = PatternRule{T}(l, r)
PatternRule(l::L, r::R) where {L,R} = PatternRule{promote_type(L,R)}(l, r)
PatternRule((l, r)::Pair) = PatternRule(l, r)
Base.convert(::Type{PR}, p::Pair) where {PR<:PatternRule} = PR(p)
Base.convert(::Type{Rule{T}}, p::Pair) where {T} = convert(PatternRule{T}, p)
Base.convert(::Type{Rule}, p::Pair) = convert(PatternRule, p)
Base.convert(::Type{Pair}, r::PatternRule) = r.left => r.right
Base.:(==)(a::PatternRule, b::PatternRule) = (a.left, a.right) == (b.left, b.right)
Base.hash(p::PatternRule{T}, h::UInt) where {T} = hash((p.left, p.right), hash(PatternRule{T}, h))
Base.map(f, r::PatternRule{T}) where {T} = PatternRule{T}(f(r.left), f(r.right))
function normalize(t::T, r::PatternRule{U}) where {U,T<:U}
    Θ = match(r.left, t) |> collect
    isempty(Θ) && return t

    σᵢ = findfirst(_preds_match(r.ps), Θ)
    σᵢ === nothing && return t
    σ = Θ[σᵢ]

    replace(r.right, σ)
end
_preds_match(ps, σ) = all(p -> p(σ), ps)
_preds_match(ps) = Base.Fix1(_preds_match, ps)

struct EvalRule <: Rule{Term}
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


struct OrderRule <: Rule{Term}
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


struct DiffRule <: Rule{Term} end
normalize(t::Term, r::DiffRule) = Term(normalize(get(t), r))
function normalize(ex::Expr, r::DiffRule)
    fn = ex.args[1]
    fn === diff && length(ex.args) == 3 || return ex
    f, x = ex.args[[2, 3]]
    is_ground(f) && return :($zero($f))
    ex
end
normalize(x, ::DiffRule) = x
