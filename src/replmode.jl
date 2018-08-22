module replmode

using ReplMaker

function rewrite_parser(s)
    ex = Meta.parse(s)
    quote
        normalize(@term $ex)
    end
end

initrepl(rewrite_parser, 
         prompt_text="@term> ",
         prompt_color = :blue, 
         start_key='=', 
         mode_name="Rewrite_mode")

end
