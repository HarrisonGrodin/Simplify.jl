export Arity, Image, Signature, Closure
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
isvalid(i::Image{<:Union{Symbolic,Variable,Expr}}, ctx::Context) =
    invoke(isvalid, Tuple{Property,Context}, i, ctx)
isvalid(i::Image, ctx::Context) =
    i.ex ∈ i.S || invoke(isvalid, Tuple{Property,Context}, i, ctx)
implies(i::Image, i′::Image, ::Context) = i.ex == i′.ex && i.S ⊆ i′.S

"""
    Signature <: Property

Representation of function `f` having signature `(x₁, x₂, …, xₙ) -> y` given `xᵢ` and `y` as
image sets, `Signature(f, [x₁, x₂, …, xₙ], y)`.
"""
struct Signature <: Property
    f
    args::Vector{AbstractSet}
    S::AbstractSet
end
implies(sig::Signature, a::Arity, ::Context) = a.f == sig.f && a.n == length(sig.args)
implies(sig::Signature, i::Image{Expr}, ctx::Context) =
    sig.S ⊆ i.S && i.ex.head === :call && length(i.ex.args) == 1 + length(sig.args) &&
        sig.f == i.ex.args[1] && all(isvalid(ctx), Image.(i.ex.args[2:end], sig.args))

"""
    Closure <: Property

Representation of function `f` being closed under set `S`, `Closure(f, S)`.
"""
struct Closure <: Property
    f
    S::AbstractSet
end
implies(p::Closure, q::Closure, ::Context) = p.f == q.f && q.S ⊆ p.S
implies(p::Closure, sig::Signature, ::Context) =
    p.f == sig.f && q.S ⊆ p.S && all(sig.args .⊆ Ref(p.S))
implies(p::Closure, i::Image{Expr}, ctx::Context) =
    p.S ⊆ i.S && i.ex.head === :call && !isempty(i.ex.args) &&
        p.f == i.ex.args[1] && all(isvalid(ctx), Image.(i.ex.args[2:end], Ref(p.S)))

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
