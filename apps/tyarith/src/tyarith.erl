-module(tyarith).

-export([main/1]).

usage(Progname) ->
    io:format("Usage: ~s <source>\n",
              [Progname]).

main([]) ->
    usage(escript:script_name()),
    erlang:halt(1);
main([Source]) ->
    process_file(Source).

process_file(Source) ->
    {ok, Data} = file:read_file(Source),
    {ok, Tokens, _} = tyarith_lexer:string(unicode:characters_to_list(Data)),
    {ok, Commands} = tyarith_parser:parse(Tokens),
    process_commands(Commands).

process_commands([]) -> ok;
process_commands([C | Commands]) ->
    case C of
        {eval, _Info, Term} ->
            Result = tyarith_core:eval(Term),
            io:format("~ts\n", [tyarith_syntax:format_term(Result)])
    end,
    process_commands(Commands).
