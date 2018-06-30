export rules

rules(set::Symbol=:STANDARD, args...; kwargs...) = rules(Val(set), args...; kwargs...)


rules(::Val{:STANDARD}) = @term PAIRS [
    x + 0 => x,
    0 + x => x,
]
