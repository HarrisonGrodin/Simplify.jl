module Completion

using Base: setindex
using Rewrite
using Rewrite: Rule, PatternRule

export complete


maxindex(x::Variable) = x.index
maxindex(xs) = isempty(xs) ? 0 : maximum(maxindex, xs)

rename(n) = Base.Fix2(rename, n)
rename(x::Variable, n) = Variable(x.name, x.index + n)
rename(xs, n) = map(x -> rename(x, n), xs)

_critical_pairs(::Function, ::Pair, rs) = []
function _critical_pairs(rebuild::Function, (l, r)::Pair{Fn}, rs)
    result = Tuple{Term,Term}[]
    for (l′, r′) ∈ rs
        σ = unify(l, l′)
        σ === nothing && continue
        push!(result, (σ(r), σ(rebuild(r′))))
    end

    for (i, t) ∈ enumerate(l)
        cps = _critical_pairs(t => r, rs) do s
            rebuild(setindex(l, s, i))
        end
        append!(result, cps)
    end

    result
end

function critical_pairs(rule::PatternRule, rs::TRS)
    m = 1 + maxindex(rs)
    rule = rename(rule, m)
    _critical_pairs(identity, rule.left => rule.right, rs)
end
critical_pairs(rs::TRS, rule::PatternRule) = critical_pairs(rs, TRS(rule))
critical_pairs(r₁::PatternRule, r₂::PatternRule) = critical_pairs(r₁, TRS(r₂))

critical_pairs(rs, rs′) = [map(r -> critical_pairs(r, rs′), rs)...;]



function add_rule!(rule::PatternRule, es, ss, rs)
    l, r = rule

    for us ∈ (ss, rs)
        for _ ∈ 1:length(us)
            (m, n) = popfirst!(us)

            m′ = normalize(m, TRS(rule))
            if m′ == m
                n′ = normalize(n, [TRS(rule); rs; ss])
                push!(us, m => n′)
            else
                push!(es, (m′, n))
            end
        end
    end

    push!(ss, rule)

    es, ss, rs
end


function orient!(>ᵣ, es, ss, rs)

    while !isempty(es)
        (m, n) = pop!(es)
        (m′, n′) = normalize([rs; ss]).((m, n))
        m′ == n′ && continue
        m′ >ᵣ n′ && (add_rule!(PatternRule{Term}(m′, n′), es, ss, rs); continue)
        n′ >ᵣ m′ && (add_rule!(PatternRule{Term}(n′, m′), es, ss, rs); continue)

        @warn "Unable to determine preferred form" s=m′ t=n′
        return nothing
    end

    (ss, rs)
end


size(xs) = 1 + (isempty(xs) ? 0 : sum(size, xs))
choose!(rs) = choose!(size, rs)
function choose!(f, rs)
    isempty(rs) && throw(ArgumentError("Cannot choose from an empty list"))
    i = argmin(map(size, rs))
    r = rs[i]
    deleteat!(rs, i)
    r
end


function complete!(>ᵣ, es)
    ss = TRS()
    rs = TRS()

    while true
        orient!(>ᵣ, es, ss, rs) === nothing && return nothing
        isempty(ss) && break
        rule = choose!(ss)
        es = [critical_pairs.((rule, rs, rule), (rs, rule, rule))...;]
        push!(rs, rule)
    end

    rs
end
complete(>ᵣ, es) = complete!(>ᵣ, copy(es))

include("orders.jl")

end # module
