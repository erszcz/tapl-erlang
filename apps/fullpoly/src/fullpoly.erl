-module(fullpoly).

-export([main/1]).

-define(core,   fullpoly_core).
-define(lexer,  fullpoly_lexer).
-define(parser, fullpoly_parser).
-define(syntax, fullpoly_syntax).

-type binding()     :: ?syntax:binding().
-type command()     :: ?syntax:command().
-type context()     :: ?syntax:context().
-type info()        :: ?syntax:info().
-type uvar_gen()    :: ?core:uvar_gen().
-type constraints() :: ?core:constraints().

usage(Progname) ->
    io:format("Usage: ~s <source>\n",
              [Progname]).

-spec main([string()]) -> ok.
main([]) ->
    usage(escript:script_name()),
    erlang:halt(1);
main([Source]) ->

    %dbg:tracer(process, {fun (T, ok) -> io:format("~p\n\n", [T]) end, ok}),
    %dbg:p(all, call),
    %dbg:tpl(fullpoly_core, x),

    process_file(Source, ?syntax:empty_context(), ?core:uvar_gen(), ?core:empty_constraints()).

process_file(Source, Ctx, NextUVar, Cs) ->
    {ok, Data} = file:read_file(Source),
    {ok, Tokens, _} = ?lexer:string(unicode:characters_to_list(Data)),
    {ok, Parser} = ?parser:parse(Tokens),
    {Commands, _NewCtx} = Parser(Ctx),
    process_commands(Ctx, NextUVar, Cs, Commands).

-spec process_commands(context(), uvar_gen(), constraints(), [command()]) -> ok.
process_commands(Ctx, NextUVar, Cs, Commands) ->
    lists:foldl(fun process_command/2, {Ctx, NextUVar, Cs}, Commands),
    ok.

-spec process_command(command(), {context(), uvar_gen(), constraints()}) -> context().
process_command(Command, {Ctx, NextUVar, Cs}) ->
    case Command of
        {eval, Info, T} ->
            {TyT, NextUVar1, CsT} = ?core:recon(Ctx, NextUVar, T),
            T_ = ?core:eval(Ctx, T),
            Cs1 = ?core:combine_constraints(Cs, CsT),
            Cs2 = ?core:unify(Info, Ctx, "Could not simplify constraints", Cs1),
            io:format("~ts : ~ts\n",
                      [?syntax:format_doc(?syntax:prettypr_a_term(true, Ctx, T_), #{}),
                       ?syntax:format_type(Ctx, ?core:apply_subst(Cs2, TyT))]),
            {Ctx, NextUVar1, Cs2};
        {bind, Info, X, Bind} ->
            io:format("~ts ~ts\n", [X, ?syntax:format_binding(Ctx, Bind)]),
            {?syntax:add_binding(Ctx, X, Bind), ?core:uvar_gen(), Cs}
    end.
