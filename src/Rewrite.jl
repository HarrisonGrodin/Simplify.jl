module Rewrite

export Completion


include("types.jl")
include("property.jl")
include("match.jl")
include("rule.jl")

include("context.jl")
include("rules.jl")

include("completion/Completion.jl")
using .Completion

end # module
