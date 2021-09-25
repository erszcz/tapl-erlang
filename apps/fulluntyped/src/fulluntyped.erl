-module(fulluntyped).

-export([main/1]).

usage(Progname) ->
    io:format("Usage: ~s <source>\n",
              [Progname]).

-spec main([string()]) -> ok.
main([]) ->
    usage(escript:script_name()),
    erlang:halt(1);
main([Source]) ->
    process_file(Source).

process_file(Source) ->
    {ok, Data} = file:read_file(Source),
    {ok, Tokens, _} = fulluntyped_lexer:string(unicode:characters_to_list(Data)),
    {ok, Commands} = fulluntyped_parser:parse(Tokens),
    process_commands(Commands).

process_commands([]) -> ok;
process_commands([C | Commands]) ->
    case C of
        {eval, _Info, Term} ->
            Result = fulluntyped_core:eval(Term),
            io:format("~ts\n", [fulluntyped_syntax:format_term(Result)])
    end,
    process_commands(Commands).
