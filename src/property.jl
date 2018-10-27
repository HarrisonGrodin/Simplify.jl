export Associative, Commutative, Image
export isvalid

import Base: isvalid


abstract type Property end

struct Associative <: Property
    f
end
struct Commutative <: Property
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
