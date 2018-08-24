using ReplMaker

if isdefined(Base, :active_repl)
    initrepl(
        prompt_text  = "@term> ",
        prompt_color = :cyan, 
        start_key    = '=', 
        mode_name    = "Rewrite_mode",
    ) do s
        exs = [Meta.parse(i) for i in split(s, "with")]
        :(normalize((@term $(exs[1])), $(exs[2])))
    end
end

