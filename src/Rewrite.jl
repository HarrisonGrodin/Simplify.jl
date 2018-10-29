module Rewrite


# Base types
include("types.jl")

# Rule and context logic
include("rule.jl")
include("context.jl")

# Matching and unification
include("properties.jl")
include("match.jl")

# Default algebraic setup
include("rules.jl")
include("algebra.jl")

# Utility functions
include("utils.jl")

end # module
