create_id(Line, Chars) ->
    case reserved_word(Chars) of
        '__not_a_reserved_word__' ->
            case Chars of
                [C | _] when C >= $A andalso C =< $Z ->
                    {ucid, Line, Chars};
                _ ->
                    {lcid, Line, Chars}
            end;
        Token ->
            {Token, Line, Chars}
    end.

reserved_word(Chars) ->
    case Chars of
        %% Keywords
        "if" -> 'if';
        "then" -> 'then';
        "else" -> 'else';
        "true" -> 'true';
        "false" -> 'false';
        "succ" -> 'succ';
        "pred" -> 'pred';
        "iszero" -> 'iszero';

        %% Symbols
        "_" -> uscore;
        "'" -> apostrophe;
        "\"" -> dquote;
        "!" -> bang;
        "#" -> hash;
        "$" -> triangle;
        "*" -> star;
        "|" -> vbar;
        "." -> dot;
        ";" -> semi;
        "," -> comma;
        "/" -> slash;
        ":" -> colon;
        "::" -> coloncolon;
        "=" -> eq;
        "==" -> eqeq;
        "[" -> lsquare; 
        "<" -> lt;
        "{" -> lcurly; 
        "(" -> lparen; 
        "<-" -> leftarrow; 
        "{|" -> lcurlybar; 
        "[|" -> lsquarebar; 
        "}" -> rcurly;
        ")" -> rparen;
        "]" -> rsquare;
        ">" -> gt;
        "|}" -> barrcurly;
        "|>" -> bargt;
        "|]" -> barrsquare;

        %% Special compound symbols
        ":=" -> coloneq;
        "->" -> arrow;
        "=>" -> darrow;
        "==>" -> ddarrow;

        _ -> '__not_a_reserved_word__'
    end.
