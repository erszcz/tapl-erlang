-module(fulluntyped).

-export([main/1]).

-type command() :: fulluntyped_syntax:command().
-type context() :: fulluntyped_syntax:context().

-define(core,   fulluntyped_core).
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
    %dbg:tracer(process, {fun (Trace, ok) -> io:format("~p\n", [Trace]) end, ok}),
    dbg:p(all, call),
    %dbg:tpl(?core, eval, x),
    %dbg:tpl(?core, eval1, x),
    %dbg:tpl(?syntax, term_shift_above, x),
    %dbg:tpl(?syntax, term_, x),
    %dbg:tpl(?syntax, context_length, x),
    %dbg:tpl(?syntax, name_to_index, x),
    %dbg:tpl(?syntax, get_binding, x),
    dbg:tpl(?MODULE, process_command, x),

    process_file(Source, ?syntax:empty_context()),
    timer:sleep(500),
    ok.

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
            T_ = fulluntyped_core:eval(Ctx, T),
            %io:format("~p ~p\n", [Ctx, T_]),
            io:format("~ts\n", [?syntax:format_term(Ctx, T_)]),
            Ctx;
        {bind, _Info, X, Bind} ->
            Bind_ = fulluntyped_core:eval_binding(Ctx, Bind),
            io:format("~ts ~ts\n", [X, ?syntax:format_binding(Ctx, Bind_)]),
            R = ?syntax:add_binding(Ctx, X, Bind_),
            %io:format("new ctx: ~p\n", [R]),
            R
    end.
