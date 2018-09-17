export Flat, Orderless

abstract type Property end

struct Standard  <: Property end
struct Flat      <: Property end
struct Orderless <: Property end

function flatten!(head, ex::Expr)
	ex.head === :call   || return
	ex.args[1] === head || return

	args = ex.args
	i = 2
	while i ≤ length(args)
		arg = flatten!(head, args[i])
		arg !== nothing && splice!(args, i, arg.args[2:end])
		i += 1
	end

	ex
end
flatten!(head, x) = nothing
flatten(ex::Expr) = flatten!(ex.args[1], copy(ex))


property(::Type{<:Property}, x) = nothing
hasproperty(P::Type{<:Property}, t::Term) = hasproperty(P, get(t))
hasproperty(P::Type{<:Property}, x) = property(P, x) !== nothing
