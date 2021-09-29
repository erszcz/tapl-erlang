%% IMPORTANT! This module uses the process dictionary!

-export([reset/0]).

%% Reset this module's state in the calling process' dictionary.
%% Useful when playing with the lexer in the shell.
-spec reset() -> ok.
reset() ->
    erlang:erase(col),
    ok.

loc(Line, Chars) ->
    Col = update_column(Chars),
    {Line, Col}.

reset_column() ->
    erlang:put(col, 1).

update_column(Chars) ->
    case erlang:get(col) of
        undefined ->
            reset_column(),
            update_column(Chars);
        Col ->
            Len = case string:tokens(Chars, "\n") of
                      [] -> 0;
                      Lines -> length(lists:last(Lines))
                  end,
            erlang:put(col, Col + Len),
            Col
    end.

create_id(Line, Chars) ->
    case reserved_word(Chars) of
        '__not_a_reserved_word__' ->
            case Chars of
                [C | _] when C >= $A andalso C =< $Z ->
                    {ucid, loc(Line, Chars), Chars};
                _ ->
                    {lcid, loc(Line, Chars), Chars}
            end;
        Token ->
            {Token, loc(Line, Chars)}
    end.

reserved_word(Chars) ->
    case Chars of
        %% Keywords
        "if" -> 'if';
        "then" -> 'then';
        "else" -> 'else';
        "true" -> 'true';
        "false" -> 'false';
        "lambda" -> 'lambda';
        "timesfloat" -> 'timesfloat';
        "succ" -> 'succ';
        "pred" -> 'pred';
        "iszero" -> 'iszero';
        "let" -> 'let';
        "in" -> 'in';

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
