Base.show(io::IO, x::Variable) = printstyled(io, x.name; color = :light_cyan)
