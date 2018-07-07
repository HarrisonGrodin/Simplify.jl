module SymReduce

export Patterns, @term, normalize


include("patterns/Patterns.jl")
using .Patterns

include("rules.jl")


normalize(rs) = Base.Fix2(normalize, rs)
normalize(::Term, ::R) where {R<:Rule} = error("normalize undefined for rule type $R")
normalize(t::Term, set::Symbol) = normalize(t, rules(set))
function normalize(t::Term, rs)
    while true
        t = map(normalize(rs), t)
        t′ = foldl(normalize, rs; init=t)
        t == t′ && return t
        t = t′
    end
end
normalize(t::Term) = normalize(t, rules())

end # module
