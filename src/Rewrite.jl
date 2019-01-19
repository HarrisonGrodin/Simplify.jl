module Rewrite


# Base types
include("types.jl")

# Rule and context logic
include("rule.jl")
include("context.jl")
include("rules.jl")

# Matching
include("match.jl")

# Utility functions
include("utils.jl")

end # module
