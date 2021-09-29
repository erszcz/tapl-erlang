-module(fulluntyped).

-export([main/1]).

-type command() :: fulluntyped_syntax:command().
-type context() :: fulluntyped_syntax:context().

-define(lexer,  fulluntyped_lexer).
-define(parser, fulluntyped_parser).
-define(syntax, fulluntyped_syntax).

usage(Progname) ->
    io:format("Usage: ~s <source>\n",
              [Progname]).

-spec main([string()]) -> ok.
main([]) ->
    usage(escript:script_name()),
    erlang:halt(1);
main([Source]) ->
    process_file(Source, ?syntax:empty_context()).

process_file(Source, Ctx) ->
    {ok, Data} = file:read_file(Source),
    {ok, Tokens, _} = ?lexer:string(unicode:characters_to_list(Data)),
    {ok, Parser} = ?parser:parse(Tokens),
    {Commands, _NewCtx} = Parser(Ctx),
    process_commands(Ctx, Commands).

-spec process_commands(context(), [command()]) -> ok.
process_commands(Ctx, Commands) ->
    lists:foldl(fun process_command/2, Ctx, Commands),
    ok.

-spec process_command(command(), context()) -> context().
process_command(Command, Ctx) ->
    case Command of
        {eval, _Info, Term} ->
            Result = fulluntyped_core:eval(Ctx, Term),
            io:format("~ts\n", [?syntax:format_term(Result)]),
            Ctx;
        {bind, _Info, X, Bind} ->
            Bind_ = fulluntyped_core:eval_binding(Ctx, Bind),
            io:format("~ts ~ts\n", [X, ?syntax:format_binding(Ctx, Bind_)]),
            ?syntax:add_binding(Ctx, X, Bind_)
    end.
