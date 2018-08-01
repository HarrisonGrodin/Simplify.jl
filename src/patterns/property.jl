export Property
export Flat, Orderless


abstract type Property end

struct FlatProp <: Property end
struct OrderlessProp <: Property end

const Flat = FlatProp()
const Orderless = OrderlessProp()


hasproperty(::Property, x) = false
