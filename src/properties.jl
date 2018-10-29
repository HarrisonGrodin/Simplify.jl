export Arity, Image
export Associative, Commutative


"""
    Arity <: Property

Representation of function `f` having the arity `n`, `Arity(f, n)`.
"""
struct Arity <: Property
    f
    n::Int
end

"""
    Image <: Property

Representation of expression `ex` having the mathematical image `S::AbstractSet`,
`Image(ex, S)`.
"""
struct Image{T} <: Property
    ex::T
    S::AbstractSet
end
isvalid(i::Image, ctx::Context) = i.ex ∈ i.S || invoke(isvalid, Tuple{Property,Context}, i, ctx)
implies(i::Image, i′::Image, ::Context) = i.ex == i′.ex && i.S ⊆ i′.S

"""
    Associative <: Property

Representation of function `f` having the associative property, `Associative(f)`.
"""
struct Associative <: Property
    f
end
implies(p::Associative, a::Arity, ::Context) = a.n == 2 && a.f == p.f

"""
    Commutative <: Property

Representation of function `f` having the commutative property, `Commutative(f)`.
"""
struct Commutative <: Property
    f
end
