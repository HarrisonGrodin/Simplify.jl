export Variable


struct Variable <: AbstractExpr
    name::Symbol
end
Base.match(x::Variable, ex::AbstractExpr) = Substitution(x => ex)
Base.replace(x::Variable, sub) = get(sub, x, x)

