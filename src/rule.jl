export TermRewritingSystem, TRS
export normalize


abstract type Rule{T} end


struct AbstractRewritingSystem{T}
    rules::Vector{Rule{T}}
end
AbstractRewritingSystem{T}(rs::Union{Rule,Pair}...) where {T} =
    AbstractRewritingSystem{T}(collect(rs))
const TermRewritingSystem = AbstractRewritingSystem{Term}
const TRS = TermRewritingSystem
Base.union(R₁::TRS, R₂::TRS) = TRS([R₁.rules; R₂.rules])
Base.vcat(trss::TRS...) = TermRewritingSystem([(trs.rules for trs ∈ trss)...;])
Base.iterate(trs::TRS) = iterate(trs.rules)
Base.iterate(trs::TRS, state) = iterate(trs.rules, state)


normalize(trs::TermRewritingSystem) = Base.Fix2(normalize, trs)
function normalize(t::Term, trs::TermRewritingSystem)
    while true
        t = map(normalize(trs), t)
        t′ = foldl(normalize, trs; init=t)
        t == t′ && return t
        t = t′
    end
end
normalize(::T, ::R) where {T,R<:Rule} = error("normalize undefined for rule type $R on term type $T")
normalize(t::Term, set::Symbol) = normalize(t, rules(set))
normalize(t::Term) = normalize(t, rules())



struct DivergentError <: Exception end
Base.showerror(io::IO, ::DivergentError) = print(io, "DivergentError: divergent normalization paths")


struct PatternRule{T} <: Rule{T}
    left::T
    right::T
end
PatternRule{T}((l, r)::Pair{<:T,<:T}) where {T} = PatternRule{T}(l, r)
PatternRule(l::L, r::R) where {L,R} = PatternRule{promote_type(L,R)}(l, r)
PatternRule((l, r)::Pair) = PatternRule(l, r)
Base.convert(::Type{PR}, p::Pair) where {PR<:PatternRule} = PR(p)
Base.convert(::Type{Rule{T}}, p::Pair) where {T} = convert(PatternRule{T}, p)
Base.convert(::Type{Rule}, p::Pair) = convert(PatternRule, p)
function Base.iterate(r::PatternRule, state=:left)
    state === :left  && return (r.left, :right)
    state === :right && return (r.right, nothing)
    nothing
end
function normalize(t::T, (l, r)::PatternRule{U}) where {U,T<:U}
    Θ = match(l, t)
    isempty(Θ) && return t
    xs = Set(replace(r, σ) for σ ∈ Θ)
    length(xs) == 1 || throw(DivergentError())
    first(xs)
end

struct EvalRule <: Rule{Term}
    name::Symbol
    f
end
EvalRule(f::Function) = EvalRule(nameof(f), f)
(r::EvalRule)(args::Constant...) = Constant(r.f(get.(args)...))
function normalize(fn::Fn, r::EvalRule)
    fn.name == r.name || return fn

    if hasproperty(Flat, fn)
        o = property(Orderless, fn)
        if o !== nothing
            ordered = _apply_flat!(r, collect(o.ordered))
            ord_inds = findall(x -> x isa Constant, ordered)
            length(ord_inds) > 1 && throw(DivergentError())

            orderless = collect(o.orderless)
            inds = findall(x -> x isa Constant, orderless)
            if !isempty(ord_inds)
                ind = first(ord_inds)
                ordered[ind] = r(ordered[ind], orderless[inds]...)
            else
                isempty(inds) || push!(ordered, r(orderless[inds]...))
            end
            deleteat!(orderless, inds)

            args = [ordered; orderless]
        else
            args = _apply_flat!(r, collect(fn))
        end

        length(args) == 1 && return first(args)
        return Fn(fn.name, args...)
    end

    all_constants(fn...) || return fn
    r(fn...)
end
normalize(t::Term, ::EvalRule) = t
function _apply_flat!(r::EvalRule, args)
    i = firstindex(args)

    while i ≤ lastindex(args) - 1
        a, b = args[i:i+1]

        if a isa Constant && b isa Constant
            deleteat!(args, i)
            args[i] = r(a, b)
        else
            i += 1
        end
    end

    args
end
all_constants(::Constant...) = true
all_constants(::Term...) = false
