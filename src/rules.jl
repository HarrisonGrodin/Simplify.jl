_strategy(::Val{S}) where {S} = string(S)
macro term(v::Val, ex)
    strategy = _strategy(v)
    :(throw(ArgumentError("Undefined @term strategy: " * $strategy)))
end
macro term(strategy::Symbol, expr)
    esc(:(@term $(Val(strategy)) $expr))
end

macro term(::Val{:RULES}, ex)
    args = map(ex.args) do rule
        @assert length(rule.args) == 3 && rule.args[1] == :(=>)
        pair = rule
        ps = []

        @assert pair.head == :call
        p, a, b = pair.args
        @assert p == :(=>)

        esc(:($Rule(@term($a), @term($b))))
    end
    :(Rules($Rule[$(args...)]))
end
