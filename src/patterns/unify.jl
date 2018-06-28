export unify, match


solve(σ::Substitution, (x, y)::Tuple{Variable,Variable}, ms...) =
    x == y ? solve(σ, ms...) : eliminate(σ, (x, y), ms)
solve(σ::Substitution, (x, t)::Tuple{Variable,Term}, ms...) = eliminate(σ, (x, t), ms)
solve(σ::Substitution, (t, x)::Tuple{Term,Variable}, ms...) = eliminate(σ, (x, t), ms)
solve(σ::Substitution, (f, g)::Tuple{T,T}, ms...) where {T<:Fn} = solve(σ, zip(f, g)..., ms...)
solve(σ::Substitution, ms...) = nothing
solve(σ::Substitution) = σ
solve(ms...) = solve(Substitution(), ms...)

function eliminate(σ::Substitution, (x, t)::Tuple{Variable,Term}, ms)
    occursin(x, t) && return nothing

    xt = Substitution(x => t)
    solve(xt ∘ xt(σ), xt.(ms)...)
end

unify(t1::Term, t2::Term) = solve((t1, t2))


function matches(σ::Substitution, (x, y)::Tuple{Variable,Variable}, ms...)
    haskey(σ, x) || return matches(Substitution(x => y) ∘ σ, ms...)
    σ(x) == y ? matches(σ, ms...) : nothing
end
function matches(σ::Substitution, (x, y)::Tuple{Variable,Term}, ms...)
    haskey(σ, x) || return matches(Substitution(x => y) ∘ σ, ms...)
    σ(x) == y ? matches(σ, ms...) : nothing
end
matches(σ::Substitution, (f, g)::Tuple{T,T}, ms...) where {T<:Fn} = matches(σ, zip(f, g)..., ms...)
matches(σ::Substitution, ms...) = nothing
matches(σ::Substitution) = σ
matches(ms...) = matches(Substitution(), ms...)

Base.match(t1::Term, t2::Term) = matches((t1, t2))
