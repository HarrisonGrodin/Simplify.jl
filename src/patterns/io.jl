Base.show(io::IO, x::Variable) = printstyled(io, x.name; color = :light_cyan)
function Base.show(io::IO, fn::Fn{F}) where {F}
    printstyled(io, F; color = :yellow)
    print(io, "(")
    isempty(fn.args) || join(io, fn.args, ", ")
    print(io, ")")
end
