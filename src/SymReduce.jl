module SymReduce

export Patterns, @term
export normalize
export Completion, complete


include("patterns/Patterns.jl")
using .Patterns

include("rules.jl")


normalize(rs) = Base.Fix2(normalize, rs)
function normalize(t::Term, (l, r)::Pair)
    σ = match(l, t)
    σ === nothing && return t
    σ(r)
end
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


include("completion/Completion.jl")
using .Completion

end # module
