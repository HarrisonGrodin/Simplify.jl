_is_constant(t) = typeof(t) ∉ [Expr, Symbolic, Variable]

vars(t::Term) = vars(get(t))
vars(x::Variable) = [x]
vars(t::Expr) = [vars.(t.args)...;]
vars(x) = []
