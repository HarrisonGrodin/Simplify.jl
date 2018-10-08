module Rewrite


# Base types
include("types.jl")
include("property.jl")

# Matching and unification
include("match.jl")

# Rule and context logic
include("rule.jl")
include("context.jl")

# Default algebraic setup
include("algebra.jl")
include("rules.jl")

# Utility functions
include("utils.jl")

end # module
