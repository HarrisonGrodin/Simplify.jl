export rules

function rules(set::Symbol = :STANDARD)
    haskey(REWRITE_SYSTEMS, set) || throw(ArgumentError("Unknown rule set: $set"))
    REWRITE_SYSTEMS[set]
end

const REWRITE_SYSTEMS = Dict{Symbol,Any}(
    :STANDARD => @term PAIRS [
        x + 0 => x,
        0 + x => x,
    ]
)
