module Rewrite

export Completion


# Base types
include("types.jl")
include("property.jl")

# Matching and unification
include("match.jl")

# Rule and context logic
include("rule.jl")
include("context.jl")

# List of rules
include("rules.jl")

# Utility functions
include("utils.jl")

# include("completion/Completion.jl")
# using .Completion

end # module
