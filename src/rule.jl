using DiffRules

export Rules, Rule
export normalize


abstract type AbstractRule end


struct Rules
    rules::Vector{AbstractRule}
    Rules(rs::Vector{<:AbstractRule}) = new(rs)
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


struct Rule <: AbstractRule
    left::Term
    right::Term
end
Base.convert(::Type{Rule}, (l,r)::Pair) = Rule(l, r)
Base.convert(::Type{AbstractRule}, p::Pair) = convert(Rule, p)
function normalize(t::Term, r::Rule)
    σ = match(r.left, t)
    σ === nothing && return t

    return replace(r.right, σ)
end
