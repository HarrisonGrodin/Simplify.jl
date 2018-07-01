module Completion

using Base: setindex
using SymReduce: normalize
using SymReduce.Patterns

export complete


maxindex(x::Variable) = x.index
maxindex(xs) = isempty(xs) ? UInt(0) : maximum(maxindex, xs)

rename(n::Integer) = Base.Fix2(rename, n)
rename(x::Variable, n::Integer) = Variable(x.name, x.index + n)
rename(f::Fn, n::Integer) = map(x -> rename(x, n), f)
rename(t::Term, ::Integer) = t
rename((a, b)::Pair{A,B}, n::Integer) where {A,B} = Pair{A,B}(rename(a, n), rename(b, n))

_critical_pairs(::Function, ::Pair, rs) = []
function _critical_pairs(rebuild::Function, (l, r)::Pair{<:Fn}, rs)
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

function critical_pairs(rule::Pair, rs)
    m = 1 + maxindex(rs)
    _critical_pairs(identity, rename(rule, m), rs)
end
critical_pairs(rs, rule::Pair) = critical_pairs(rs, [rule])
critical_pairs(r₁::Pair, r₂::Pair) = critical_pairs(r₁, [r₂])

critical_pairs(rs, rs′) = [map(r -> critical_pairs(r, rs′), rs)...;]



function add_rule!((l, r)::Pair, es, ss, rs)

    for us ∈ (ss, rs)
        for _ ∈ 1:length(us)
            (m, n) = popfirst!(us)

            m′ = normalize(m, [l => r])
            if m′ == m
                n′ = normalize(n, [l => r; rs; ss])
                push!(us, m => n′)
            else
                push!(es, (m′, n))
            end
        end
    end

    push!(ss, l => r)

    es, ss, rs
end


function orient!(>ᵣ, es, ss, rs)

    while !isempty(es)
        (m, n) = pop!(es)
        (m′, n′) = normalize([rs; ss]).((m, n))
        m′ == n′ && continue
        m′ >ᵣ n′ && (add_rule!(m′ => n′, es, ss, rs); continue)
        n′ >ᵣ m′ && (add_rule!(n′ => m′, es, ss, rs); continue)

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
    ss = Pair[]
    rs = Pair[]

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
