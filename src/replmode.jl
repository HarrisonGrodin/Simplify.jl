using ReplMaker

if isdefined(Base, :active_repl)
    initrepl(
        prompt_text  = "@term> ",
        prompt_color = :blue, 
        start_key    = '=', 
        mode_name    = "Rewrite_mode",
    ) do s
        ex = Meta.parse(s)
        :(normalize((@term $(ex)), ruleset...))
    end
end
