-module(fullequirec).

-export([main/1]).

-type binding() :: fullequirec_syntax:binding().
-type command() :: fullequirec_syntax:command().
-type context() :: fullequirec_syntax:context().
-type info()    :: fullequirec_syntax:info().

-define(core,   fullequirec_core).
-define(lexer,  fullequirec_lexer).
-define(parser, fullequirec_parser).
-define(syntax, fullequirec_syntax).

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
            T_ = fullequirec_core:eval(Ctx, T),
            io:format("~ts : ~ts\n", [?syntax:format_doc(?syntax:prettypr_a_term(true, Ctx, T_), #{}),
                                      ?syntax:format_type(Ctx, TyT)]),
            Ctx;
        {bind, Info, X, Bind0} ->
            Bind = check_binding(Info, Ctx, Bind0),
            Bind_ = fullequirec_core:eval_binding(Ctx, Bind),
            io:format("~ts ~ts\n", [X, format_binding_type(Ctx, Bind_)]),
            ?syntax:add_binding(Ctx, X, Bind_)
    end.

-spec check_binding(info(), context(), binding()) -> binding().
check_binding(Info, Ctx, B) ->
    case B of
        name_bind -> name_bind;
        {var_bind, TyT} -> {var_bind, TyT};
        {tm_abb_bind, T, none} -> {tm_abb_bind, T, ?core:type_of(Ctx, T)};
        {tm_abb_bind, T, TyT} ->
            TyT_ = ?core:type_of(Ctx, T),
            case ?core:types_equiv(Ctx, TyT_, TyT) of
                true ->
                    {tm_abb_bind, T, TyT};
                false ->
                    ?core:type_error(Info, "type of binding does not match declared type")
            end;
        ty_var_bind -> ty_var_bind;
        {ty_abb_bind, TyT} -> {ty_abb_bind, TyT}
    end.

-spec format_binding_type(context(), binding()) -> io_lib:chars().
format_binding_type(Ctx, B) ->
    case B of
        name_bind -> "";
        ty_var_bind -> "";
        {var_bind, TyT} ->
            io_lib:format(": ~ts", [?syntax:format_type(Ctx, TyT)]);
        {tm_abb_bind, T, MaybeTyT} ->
            case MaybeTyT of
                none -> ?syntax:format_type(Ctx, ?core:type_of(Ctx, T));
                TyT -> ?syntax:format_type(Ctx, TyT)
            end;
        {ty_abb_bind, _} -> ":: *"
    end.
