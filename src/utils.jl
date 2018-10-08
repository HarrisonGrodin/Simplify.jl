is_constant(t::Term) = is_constant(get(t))
is_constant(x) = typeof(x) ∉ [Expr, Symbolic, Variable]

is_ground(t::Term) = is_ground(get(t))
is_ground(ex::Expr) = all(is_ground, ex.args)
is_ground(::Symbolic) = false
is_ground(::Variable) = false
is_ground(x) = true


vars(t::Term) = vars(get(t))
vars(x::Variable) = [x]
vars(t::Expr) = [vars.(t.args)...;]
vars(x) = []


_image(x, S) = σ -> isvalid(Image(σ[x], S))
