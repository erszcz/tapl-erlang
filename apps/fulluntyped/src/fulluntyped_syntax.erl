-module(fulluntyped_syntax).

%% Constructors
-export([binding/1,
         command/1,
         info/1,
         term_/1]).

%% Context management
-export([add_binding/3,
         add_name/2,
         context_length/1,
         empty_context/0,
         get_binding/3,
         name_to_index/3]).

%% Substitution
-export([term_subst_top/2]).

%% Printing
-export([format_term/1, format_term/2,
         format_binding/2, format_binding/3]).

-export([term_info/1]).

-export_type([binding/0,
              context/0,
              command/0,
              term_/0]).

-include_lib("gradualizer/include/gradualizer.hrl").

%%
%%' Datatypes
%%

-type info() :: {pos_integer(), pos_integer()}.

-type token() :: {atom(), info()}
               | {atom(), info(), string()}.

-type term_() :: {true, info()}
               | {false, info()}
               | {if_, info(), term_(), term_(), term_()}
               | {var, info(), pos_integer(), non_neg_integer()}
               | {abs, info(), string(), term_()}
               | {app, info(), term_(), term_()}
               | {proj, info(), term_(), string()}
               | {record, info(), [{string(), term_()}]}
               | {float, info(), float()}
               | {timesfloat, info(), term_(), term_()}
               | {string, info(), string()}
               | {zero, info()}
               | {succ, info(), term_()}
               | {pred, info(), term_()}
               | {is_zero, info(), term_()}
               | {let_, info(), string(), term_(), term_()}.
%% TAPL `term' type, but `term()' is a builtin type in Erlang,
%% hence the name `term_()'.

-type binding() :: name_bind | {abb_bind, term_()}.

-type context() :: [{string(), binding()}].

-type command() :: {eval, info(), term()}
                 | {bind, info(), string(), binding()}.

%%.
%%' Constructors
%%

-spec info(token()) -> info().
info({_, Info}) -> Info;
info({_, Info, _Chars}) -> Info.

%% This function might seem useless, since it "doesn't do anything",
%% but in fact it gives us two guarantees:
%% - thanks to Gradualizer it provides compile-time warnings if we build an invalid term_()
%% - at runtime, it crashes if we try to build an invalid term_()
-spec term_(term_()) -> term_().
term_(T) ->
    case T of
        {true, _} -> T;
        {false, _} -> T;
        {if_, _, _, _, _} -> T;
        {var, _, _, _} -> T;
        {abs, _, _, _} -> T;
        {app, _, _, _} -> T;
        {proj, _, _, _} -> T;
        {record, _, _} -> T;
        {float, _, _} -> T;
        {timesfloat, _, _, _} -> T;
        {string, _, _} -> T;
        {zero, _} -> T;
        {succ, _, _} -> T;
        {pred, _, _} -> T;
        {is_zero, _, _} -> T;
        {let_, _, _, _, _} -> T
    end.

-spec binding(binding()) -> binding().
binding(B) ->
    case B of
        name_bind -> B;
        {abb_bind, _} -> B
    end.

-spec command(command()) -> command().
command(C) ->
    case C of
        {eval, _, _} -> C;
        {bind, _, _, _} -> C
    end.

%%.
%%' Context management
%%

-spec empty_context() -> context().
empty_context() ->
    [].

-spec context_length(context()) -> non_neg_integer().
context_length(Ctx) ->
    length(Ctx).

-spec add_binding(context(), _, binding()) -> context().
add_binding(Ctx, X, Bind) ->
    [{X, Bind} | Ctx].

-spec add_name(context(), string()) -> context().
add_name(Ctx, X) ->
    add_binding(Ctx, X, name_bind).

-spec is_name_bound(context(), _) -> boolean().
is_name_bound(Ctx, X) ->
    case lists:keyfind(X, 1, Ctx) of
        {X, _} -> true;
        false -> false
    end.

-spec pick_fresh_name(context(), string()) -> {context(), string()}.
pick_fresh_name(Ctx, X) ->
    case lists:keyfind(X, 1, Ctx) of
        false ->
            {add_name(Ctx, X), X};
        {X, _} ->
            {match, AlNum, Seq} = re:run(X, "([a-zA-Z]+)([0-9]+)", [{capture, all_but_first, list}]),
            X1 = AlNum ++ list_to_integer(integer_to_list(Seq) + 1),
            {add_name(Ctx, X1), X1}
    end.

-spec index_to_name(info(), context(), non_neg_integer()) -> string().
index_to_name(FInfo, Ctx, I) ->
    try
        {X, _} = lists:nth(I, Ctx),
        X
    catch
        error:function_clause ->
            erlang:error({variable_lookup_failure, FInfo, I, context_length(Ctx)}, [FInfo, Ctx, I])
    end.

-spec name_to_index(info(), context(), string()) -> non_neg_integer().
name_to_index(FInfo, Ctx, X) ->
    case Ctx of
        [] ->
            erlang:error({unbound_identifier, FInfo, X, Ctx});
        [{X, _} | _] ->
            0;
        [_ | CtxRest] ->
            1 + name_to_index(FInfo, CtxRest, X)
    end.

%%.
%%' Shifting
%%

-spec term_map(OnVarF, _, term_()) -> term_() when
      OnVarF :: fun((info(), _, integer(), integer()) -> term_()).
term_map(OnVarF, C, T) ->
    walk(OnVarF, C, T).

-spec walk(OnVarF, _, term_()) -> term_() when
      OnVarF :: fun((info(), _, integer(), integer()) -> term_()).
walk(OnVarF, C, T) ->
    case T of
        {true, _} ->
            T;
        {false, _} ->
            T;
        {if_, FInfo, T1, T2, T3} ->
            {if_, FInfo, walk(OnVarF, C, T1), walk(OnVarF, C, T2), walk(OnVarF, C, T3)};
        {var, FInfo, X, N} ->
            OnVarF(FInfo, C, X, N);
        {abs, FInfo, X, T2} ->
            {abs, FInfo, X, walk(OnVarF, C+1, T2)};
        {app, FInfo, T1, T2} ->
            {app, FInfo, walk(OnVarF, C, T1), walk(OnVarF, C, T2)};
        {proj, FInfo, T1, Label} ->
            {proj, FInfo, walk(OnVarF, C, T1), Label};
        {record, FInfo, Fields} ->
            {record, FInfo, [ {Label, walk(OnVarF, C, FieldT)} || {Label, FieldT} <- Fields ]};
        {float, _, _} ->
            T;
        {timesfloat, FInfo, T1, T2} ->
            {timesfloat, FInfo, walk(OnVarF, C, T1), walk(OnVarF, C, T2)};
        {string, _, _} ->
            T;
        {zero, _} ->
            T;
        {succ, FInfo, T1} ->
            {succ, FInfo, walk(OnVarF, C, T1)};
        {pred, FInfo, T1} ->
            {pred, FInfo, walk(OnVarF, C, T1)};
        {is_zero, FInfo, T1} ->
            {is_zero, FInfo, walk(OnVarF, C, T1)};
        {let_, FInfo, X, T1, T2} ->
            {let_, FInfo, X, walk(OnVarF, C, T1), walk(OnVarF, C, T2)}
    end.

-spec term_shift_above(_, _, term_()) -> term_().
term_shift_above(D, C, T) ->
    term_map(fun
                 (FInfo, C1, X, N) when X >= C1 -> {var, FInfo, X+D, N+D};
                 (FInfo, _, X, N) -> {var, FInfo, X, N+D}
             end, C, T).

-spec term_shift(_, term_()) -> term_().
term_shift(D, T) ->
    term_shift_above(D, 0, T).

-spec binding_shift(_, binding()) -> binding().
binding_shift(D, Bind) ->
    case Bind of
        name_bind -> name_bind;
        {abb_bind, T} -> {abb_bind, term_shift(D, T)}
    end.

%%.
%%' Substitution
%%

term_subst(J, S, T) ->
    term_map(fun
                 (_FInfo, C, X, _) when X == J + C -> term_shift(C, S);
                 ( FInfo, _, X, N) -> {var, FInfo, X, N}
             end, 0, T).

term_subst_top(S, T) ->
    term_shift(-1, term_subst(0, term_shift(1, S), T)).

%%.
%%' Context management (continued)
%%

-spec get_binding(info(), context(), non_neg_integer()) -> binding().
get_binding(FInfo, Ctx, I) ->
    case Ctx of
        [] ->
            erlang:error({variable_lookup_failure, FInfo, I, context_length(Ctx)}, [FInfo, Ctx, I]);
        [_|_] ->
            {_, Bind} = lists:nth(I, ?assert_type(Ctx, [T, ...])),
            binding_shift(I+1, Bind)
    end.

%%.
%%' Extracting file info
%%

-spec term_info(term_()) -> info().
term_info(T) ->
    case T of
        {true, Info} -> Info;
        {false, Info} -> Info;
        {if_, Info, _, _, _} -> Info;
        {var, Info, _, _} -> Info;
        {abs, Info, _, _} -> Info;
        {app, Info, _, _} -> Info;
        {proj, Info, _, _} -> Info;
        {record, Info, _} -> Info;
        {float, Info, _} -> Info;
        {timesfloat, Info, _, _} -> Info;
        {string, Info, _} -> Info;
        {zero, Info} -> Info;
        {succ, Info, _} -> Info;
        {pred, Info, _} -> Info;
        {is_zero, Info, _} -> Info;
        {let_, Info, _, _, _} -> Info
    end.

%%.
%%' Printing
%%

-spec small(term_()) -> boolean().
small({var, _, _, _}) -> true;
small(_) -> false.

-spec format_term(term_()) -> string().
format_term(T) -> format_term(T, #{}).

-spec format_term(term_(), map()) -> string().
format_term(T, Opts) ->
    Doc = prettypr_term(true, empty_context(), T),
    prettypr:format(Doc,
                    maps:get(paper_width, Opts, 80),
                    maps:get(line_width, Opts, 65)).

-spec prettypr_term(boolean(), context(), term_()) -> prettypr:document().
prettypr_term(_Outer, Ctx, {if_, _Info, T1, T2, T3}) ->
    prettypr:sep([
                  prettypr:par([prettypr:text("if"),
                                prettypr_term(false, Ctx, T1)], 2),
                  prettypr:par([prettypr:text("then"),
                                prettypr_term(false, Ctx, T2)], 2),
                  prettypr:par([prettypr:text("else"),
                                prettypr_term(false, Ctx, T3)], 2)
                 ]);

prettypr_term(Outer, Ctx, {abs, _Info, X, T2}) ->
    {NewCtx, X2} = pick_fresh_name(Ctx, X),
    prettypr:follow(prettypr:text(string:join(["lambda ", X2, "."], "")),
                    prettypr_term(Outer, NewCtx, T2), 2);

prettypr_term(Outer, Ctx, {let_, _Info, X, T1, T2}) ->
    prettypr:par([prettypr:text(string:join(["let ", X, " = "], "")),
                  prettypr:beside(prettypr_term(false, Ctx, T1),
                                  prettypr:text("in")),
                 prettypr_term(false, add_name(Ctx, X), T2)], 0);

prettypr_term(Outer, Ctx, T) ->
    prettypr_app_term(Outer, Ctx, T).

prettypr_app_term(_Outer, Ctx, {app, _Info, T1, T2}) ->
    prettypr:par([prettypr_app_term(false, Ctx, T1),
                  prettypr_a_term(false, Ctx, T2)], 0);

prettypr_app_term(_Outer, Ctx, {times_float, _Info, T1, T2}) ->
    prettypr:follow(prettypr:follow(prettypr:text("timesfloat"),
                                    prettypr_a_term(false, Ctx, T1)),
                    prettypr_a_term(false, Ctx, T2));

prettypr_app_term(_Outer, Ctx, {pred, _Info, T}) ->
    prettypr:follow(prettypr:text("pred"),
                    prettypr_a_term(false, Ctx, T));

prettypr_app_term(_Outer, Ctx, {is_zero, _Info, T}) ->
    prettypr:follow(prettypr:text("iszero"),
                    prettypr_a_term(false, Ctx, T));

prettypr_app_term(Outer, Ctx, T) ->
    prettypr_path_term(Outer, Ctx, T).

prettypr_path_term(_Outer, Ctx, {proj, _Info, T1, Label}) ->
    prettypr:beside(prettypr:beside(prettypr_a_term(false, Ctx, T1), prettypr:text(".")),
                    prettypr:text(Label));

prettypr_path_term(Outer, Ctx, T) ->
    prettypr_a_term(Outer, Ctx, T).

prettypr_a_term(_Outer, _Ctx, {true, _}) ->
    prettypr:text("true");

prettypr_a_term(_Outer, _Ctx, {false, _}) ->
    prettypr:text("false");

prettypr_a_term(_Outer, Ctx, {var, Info, X, N}) ->
    case context_length(Ctx) of
        N ->
            prettypr:text(index_to_name(Info, Ctx, X));
        _ ->
            prettypr:text(io_lib:format("[bad index: ~p / ~p in ~p]", [X, N, Ctx]))
    end;

prettypr_a_term(_Outer, Ctx, {record, _Info, Fields}) ->
    FieldsD = lists:join(prettypr:text(","),
                         [ prettypr:par([prettypr:text(Label),
                                         prettypr:text(" = "),
                                         prettypr_term(false, Ctx, T)], 2)
                           || {Label, T} <- Fields ]),
    prettypr:par([prettypr:text("{")] ++ FieldsD ++ [prettypr:text("}")], 2);

prettypr_a_term(_Outer, _Ctx, {float, _Info, S}) ->
    prettypr:text(io_lib:format("~p", [S]));

prettypr_a_term(_Outer, _Ctx, {string, _Info, S}) ->
    prettypr:text(io_lib:format("~p", [S]));

prettypr_a_term(_Outer, _Ctx, {zero, _}) ->
    prettypr:text("0");

prettypr_a_term(_Outer, Ctx, {succ, _, T}) ->
    prettypr_succ(Ctx, T, 1);

prettypr_a_term(_Outer, Ctx, T) ->
    prettypr:beside(prettypr:beside(prettypr:text("("),
                                    prettypr_term(false, Ctx, T)),
                    prettypr:text(")")).

prettypr_succ(_Ctx, {zero, _}, N) ->
    prettypr:text(integer_to_list(N));

prettypr_succ(Ctx, {succ, _, T}, N) ->
    prettypr_succ(Ctx, T, N+1);

prettypr_succ(Ctx, T, _N) ->
    prettypr:follow(prettypr:text("(succ"),
                    prettypr:beside(prettypr_a_term(false, Ctx, T),
                                    prettypr:text(")")), 2).

-spec format_binding(context(), binding()) -> string().
format_binding(Ctx, B) ->
    format_binding(Ctx, B, #{}).

-spec format_binding(context(), binding(), _) -> string().
format_binding(Ctx, B, Opts) ->
    case B of
        name_bind ->
            "";
        {abb_bind, T} ->
            Doc = prettypr:follow(prettypr:text("= "), prettypr_term(true, Ctx, T)),
            prettypr:format(Doc,
                            maps:get(paper_width, Opts, 80),
                            maps:get(line_width, Opts, 65))
    end.

%%.
%%' Tests
%%

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

format_term_test_() ->
    [
     ?_test(format_term( {true, 0}                                          )),
     ?_test(format_term( {false, 0}                                         )),
     ?_test(format_term( {if_, 0, {is_zero, 0, {zero, 0}},
                          {zero, 0},
                          {succ, 0, {zero, 0}}}                             )),
     ?_test(format_term( {var, 0, 1, 1}                                     )),
     ?_test(format_term( {abs, 0, "x", {true, 0}}                           )),
     ?_test(format_term( {app, 0, {true, 0}, {false, 0}}                    )),
     ?_test(format_term( {proj, 0, {true, 0}, "x"}                          )),
     ?_test(format_term( {record, 0, [{"a", {true, 0}}]}                    )),
     ?_test(format_term( {float, 0, 3.14}                                   )),
     ?_test(format_term( {times_float, 0, {float, 0, 2.0}, {float, 0, 3.0}} )),
     ?_test(format_term( {string, 0, "ala ma kota, a kot ma ale"}           )),
     ?_test(format_term( {zero, 0}                                          )),
     ?_test(format_term( {succ, 0, {zero, 0}}                               )),
     ?_test(format_term( {pred, 0, {succ, 0, {zero, 0}}}                    )),
     ?_test(format_term( {is_zero, 0, {zero, 0}}                            )),
     ?_test(format_term( {let_, 0, "a", {true, 0}, {false, 0}}              ))
    ].

format_binding_test_() ->
    [
     ?_test(format_binding( empty_context(), name_bind                      )),
     ?_test(format_binding( add_name(empty_context(), "x"),
                            {abb_bind, {var, 0, 1, 1}}                      ))
    ].

-endif. %% TEST

%%. vim: foldmethod=marker foldmarker=%%',%%.
