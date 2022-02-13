-module(fullsimple).

-export([main/1]).

-type command() :: fullsimple_syntax:command().
-type context() :: fullsimple_syntax:context().

-define(core,   fullsimple_core).
-define(lexer,  fullsimple_lexer).
-define(parser, fullsimple_parser).
-define(syntax, fullsimple_syntax).

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
        {eval, _Info, T} ->
            TyT = ?core:type_of(Ctx, T),
            T_ = fullsimple_core:eval(Ctx, T),
            io:format("~ts: ~ts\n", [?syntax:format_doc(?syntax:prettypr_a_term(true, Ctx, T_), #{}),
                                     ?syntax:format_type(Ctx, TyT)]),
            Ctx;
        {bind, _Info, X, Bind} ->
            Bind_ = fullsimple_core:eval_binding(Ctx, Bind),
            io:format("~ts ~ts\n", [X, ?syntax:format_binding(Ctx, Bind_)]),
            ?syntax:add_binding(Ctx, X, Bind_)
    end.
