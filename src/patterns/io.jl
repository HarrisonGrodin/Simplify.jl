Base.show(io::IO, x::Variable) = printstyled(io, x.name; color = :light_cyan)
function Base.show(io::IO, f::Fn)
    printstyled(io, f.head; color = :yellow)
    print(io, "(")
    isempty(f.args) || join(io, f.args, ", ")
    print(io, ")")
end
Base.show(io::IO, ::TypeSet{T}) where {T} = printstyled(io, "::$T"; color = :red)
Base.show(io::IO, c::Constant{T}) where {T} = printstyled(io, "$(c.value)::$T"; color = :green)
