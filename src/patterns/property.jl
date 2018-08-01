export Property
export Flat, Orderless


abstract type Property end

struct Flat <: Property end
struct Orderless <: Property end


hasproperty(::Property, x) = false
