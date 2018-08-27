import Base: match
using Combinatorics: combinations, permutations

export match


mutable struct Match <: AbstractSet{AbstractDict{Term,Term}}
    matches::Set{Dict{Term,Term}}
end
Match(xs::Union{Pair,Dict}...) = Match(Set(Dict.(xs)))
Base.zero(::Type{Match}) = Match()
Base.one(::Type{Match}) = Match(Dict())
Base.length(Θ::Match) = length(Θ.matches)
Base.iterate(Θ::Match) = iterate(Θ.matches)
Base.iterate(Θ::Match, state) = iterate(Θ.matches, state)
Base.push!(Θ::Match, items...) = (push!(Θ.matches, items...); Θ)
Base.copy(Θ::Match) = Match(copy(Θ.matches))
Base.union(Θ₁::Match, Θ₂::Match) = Match(union(Θ₁.matches, Θ₂.matches))
(Θ::Match)(t::Term) = Set{Term}([replace(t, σ) for σ ∈ Θ])

function Base.merge!(Θ::Match, Θs::Match...)
    for Θ′ ∈ Θs
        result = Match()
        for σ′ ∈ Θ′
            foreach(Θ) do σ
                res = copy(σ)
                for (k, v) in σ′
                    if haskey(σ, k)
                        σ[k] == v || return
                    end
                    res[k] = v
                end
                push!(result, res)
            end
        end
        Θ.matches = result
    end
    Θ
end
Base.merge(σ::Match, σs::Match...) = merge!(one(Match), σ, σs...)

"""
    match(pattern::Term, subject::Term, [Θ::Match]) -> Match

Match term `t` to `pattern`, producing a `Match` if the process succeeds. If an existing
match `Θ` is provided, it is used as the base for the returned match.

# Examples
```jldoctest
julia> match(@term(x), @term(f(x)))
Match(Set(Dict{Term,Term}[Dict(@term(x)=>@term(f(x)))]))

julia> match(@term(f(x)), @term(x))
Match(Set(Dict{Term,Term}[]))

julia> match(@term(f(x, x)), @term(f(a, b)))
Match(Set(Dict{Term,Term}[]))

julia> match(@term(f(x, x)), @term(f(a, a)))
Match(Set(Dict{Term,Term}[Dict(@term(x)=>@term(a))]))
```
"""
match(pattern::Term, subject::Term) = match(pattern, subject, one(Match))

function match(x::Variable, t::Term, Θ)
    image(t) ⊆ image(x) || return zero(Match)
    merge(Θ, Match(x => t))
end
function match(f::Fn, g::Fn, Θ)
    f.ex.head === g.ex.head || return zero(Match)
    image(g) ⊆ image(f) || return zero(Match)

    f_flat = property(Flat, f)
    g_flat = property(Flat, g)
    flat = f_flat !== nothing && g_flat !== nothing

    if flat
        match_flat(f_flat, g_flat, Θ)
    else
        match_standard(f, g, Θ)
    end
    # callback = flat ? match_flat : match_standard
    # if (g′ = property(Orderless, g)) !== nothing
    #     match_orderless(f, g′, Θ, callback)
    # elseif (f′ = property(Orderless, f)) !== nothing
    #     match_orderless(f′, g, Θ, callback)
    # else
    #     callback(f, g, Θ)
    # end
end
match(a::Term{T}, b::Term{U}, Θ) where {T,U} =
    (U <: T && get(a) == get(b)) ? Θ : zero(Match)


function match_standard(f::Fn, g::Fn, Θ)
    length(f) == length(g) || return zero(Match)

    for (x, y) ∈ zip(f, g)
        Θ = match(x, y, Θ)
    end

    Θ
end
"""
    match_flat(p::Fn, s::Fn, Θ::Match) -> Match

Match an associative function call to another associative function call, based on the
algorithm by [Krebber](https://arxiv.org/abs/1705.00907).
"""
function match_flat(p::Flat, s::Flat, Θ)
    p.name === s.name || return zero(Match)

    m, n = length(p.args), length(s.args)
    m > n && return zero(Match)
    n_free = n - m
    n_vars = count(x -> x isa Variable, p.args)
    Θᵣ = zero(Match)

    for k ∈ Iterators.product((0:n_free for i ∈ 1:n_vars)...)
        (isempty(k) ? 0 : sum(k)) == n_free || continue  # FIXME: efficiency
        i, j = 1, 1
        Θ′ = Θ
        for pₗ ∈ p.args
            l_sub = 0
            if pₗ isa Variable
                l_sub += k[j]
                j += 1
            end
            s′ = l_sub > 0 ? convert(Term, Expr(:call, p.name.ex, s[i:i+l_sub]...)) : s[i]
            Θ′ = match(pₗ, s′, Θ′)
            isempty(Θ′) && break
            i += l_sub + 1
        end
        Θᵣ = Θᵣ ∪ Θ′
    end
    Θᵣ
end
function match_orderless(p::Orderless, s::Fn, Θ, callback)
    results = map(fn -> callback(fn, s, Θ), perms(p))
    reduce(union, results)
end
function match_orderless(p::Fn, s::Orderless, Θ, callback)
    results = map(fn -> callback(p, fn, Θ), perms(s))
    reduce(union, results)
end
function match_orderless(p::Orderless, s::Orderless, Θ, callback)
    results = map(fn -> match_orderless(fn, s, Θ, callback), perms(p))
    reduce(union, results)
end
function perms(o::Orderless)
    l₊, l₋ = length(o.ordered), length(o.orderless)
    l = l₊ + l₋
    result = []

    for comb ∈ combinations(1:l, l₊)
        comb′ = setdiff(1:l, comb)
        for perm ∈ permutations(o.orderless)
            args = Array{Term}(undef, l)
            args[comb] = o.ordered
            args[comb′] = perm
            push!(result, Fn(o.name, args...; clean=false))
        end
    end

    result
end
