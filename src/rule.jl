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
normalize(t::Term, sets::Symbol...) = normalize(t, vcat(rules.(sets)...))
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
Base.convert(::Type{Pair}, r::PatternRule) = r.left => r.right
Base.:(==)(a::PatternRule, b::PatternRule) = (a.left, a.right) == (b.left, b.right)
Base.hash(p::PatternRule{T}, h::UInt) where {T} = hash((p.left, p.right), hash(PatternRule{T}, h))
function Base.iterate(r::PatternRule, state=:left)
    state === :left  && return (r.left, :right)
    state === :right && return (r.right, nothing)
    nothing
end
Base.map(f, r::PatternRule{T}) where {T} = PatternRule{T}(f(r.left), f(r.right))
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


struct DiffRule <: Rule{Term}
    diff::Symbol
    zero::Symbol
    DiffRule(diff=:diff, zero=:zero) = new(diff, zero)
end
function normalize(fn::Fn, r::DiffRule)::Term
    (fn.name, length(fn)) == (r.diff, 2) || return fn
    f, x = fn
    vars_f, vars_x = vars.((f, x))
    isempty(vars_f ∩ vars_x) && return Fn(:zero, x)
    fn
end
normalize(t::Term, ::DiffRule) = t
vars(x::Variable) = [x]
vars(t::Term) = [map(vars, collect(t))...;]
