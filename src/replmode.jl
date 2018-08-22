module REPLMode

using ReplMaker

initrepl(
    prompt_text  = "@term> ",
    prompt_color = :blue, 
    start_key    = '=', 
    mode_name    = "Rewrite_mode",
) do s
    term = convert(Term, Meta.parse(s))
    :(normalize($term))
end

end
