export Flat, Orderless, Image
export isvalid

import Base: isvalid


abstract type Property end

struct Standard <: Property end
struct Flat <: Property
    f
end
struct Orderless <: Property
    f
end
struct Image <: Property
    ex
    set::AbstractSet
end

"""
    isvalid(p::Property) -> Bool

Returns the value corresponding to whether or not the given property object `p` is valid,
given the current context.
"""
isvalid(::Property) = false
