-module(fulluntyped_syntax).

-export(['if'/4,
         succ/2,
         pred/2,
         is_zero/2,
         eval/1]).

-export([format_term/1, format_term/2]).

-export_type([command/0,
              term_/0]).

%%
%%' Datatypes
%%

-type info() :: integer().

-type term_() :: {true, info()}
               | {false, info()}
               | {'if', info(), term_(), term_(), term_()}
               | {var, info(), integer(), integer()}
               | {abs, info(), string(), term_()}
               | {app, info(), term_(), term_()}
               | {proj, info(), term_(), string()}
               | {record, info(), [{string(), term_()}]}
               | {float, info(), float()}
               | {times_float, info(), term_(), term_()}
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

-type token() :: any().

-type command() :: {eval, info(), term()}
                 | {bind, info(), string(), binding()}.

%%.
%%' Constructors
%%

-spec 'if'(token(), token(), token(), token()) -> term_().
'if'({'if', Info}, Cond, Then, Else) ->
    {'if', Info, Cond, Then, Else}.

-spec succ(token(), token()) -> term_().
succ({succ, Info}, T) ->
    {succ, Info, T}.

-spec pred(token(), token()) -> term_().
pred({pred, Info}, T) ->
    {pred, Info, T}.

-spec is_zero(token(), token()) -> term_().
is_zero({iszero, Info}, T) ->
    {is_zero, Info, T}.

-spec eval(term()) -> command().
eval(T) ->
    {eval, term_info(T), T}.

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

-spec name_to_index(info(), context(), string()) -> pos_integer().
name_to_index(FInfo, Ctx, X) ->
    case Ctx of
        [] ->
            erlang:error({identifier_is_unbound, FInfo, X});
        [X | _] ->
            0;
        [_ | Rest] ->
            1 + name_to_index(FInfo, Rest, X)
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
        {'if', FInfo, T1, T2, T3} ->
            {'if', FInfo, walk(OnVarF, C, T1), walk(OnVarF, C, T2), walk(OnVarF, C, T3)};
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
        {times_float, FInfo, T1, T2} ->
            {times_float, FInfo, walk(OnVarF, C, T1), walk(OnVarF, C, T2)};
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

get_binding(FInfo, Ctx, I) ->
    try
        {_, Bind} = lists:nth(I, Ctx),
        binding_shift(I+1, Bind)
    catch
        error:function_clause ->
            erlang:error({variable_lookup_failure, FInfo, I, context_length(Ctx)}, [FInfo, Ctx, I])
    end.

%%.
%%' Extracting file info
%%

-spec term_info(term_()) -> info().
term_info(T) ->
    case T of
        {true, Info} -> Info;
        {false, Info} -> Info;
        {'if', Info, _, _, _} -> Info;
        {var, Info, _, _} -> Info;
        {abs, Info, _, _} -> Info;
        {app, Info, _, _} -> Info;
        {proj, Info, _, _} -> Info;
        {record, Info, _} -> Info;
        {float, Info, _} -> Info;
        {times_float, Info, _, _} -> Info;
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

-spec format_term(term_()) -> string().
format_term(T) -> format_term(T, #{}).

-spec format_term(term_(), map()) -> string().
format_term(T, Opts) ->
    Doc = prettypr_term(true, T),
    prettypr:format(Doc,
                    maps:get(paper_width, Opts, 80),
                    maps:get(line_width, Opts, 65)).

-spec prettypr_term(boolean(), term_()) -> prettypr:document().
prettypr_term(_Outer, {'if', _Info, T1, T2, T3}) ->
    prettypr:sep([
                  prettypr:par([prettypr:text("if"),
                                prettypr_term(false, T1)], 2),
                  prettypr:par([prettypr:text("then"),
                                prettypr_term(false, T2)], 2),
                  prettypr:par([prettypr:text("else"),
                                prettypr_term(false, T3)], 2)
                 ]);
prettypr_term(Outer, T) ->
    prettypr_app_term(Outer, T).

prettypr_app_term(_Outer, {pred, _Info, T}) ->
    prettypr:follow(prettypr:text("pred"),
                    prettypr_a_term(false, T));
prettypr_app_term(_Outer, {is_zero, _Info, T}) ->
    prettypr:follow(prettypr:text("iszero"),
                    prettypr_a_term(false, T));
prettypr_app_term(Outer, T) ->
    prettypr_a_term(Outer, T).

prettypr_a_term(_Outer, {true, _}) ->
    prettypr:text("true");
prettypr_a_term(_Outer, {false, _}) ->
    prettypr:text("false");
prettypr_a_term(_Outer, {zero, _}) ->
    prettypr:text("0");
prettypr_a_term(_Outer, {succ, _, T}) ->
    prettypr_succ(T, 1);
prettypr_a_term(_Outer, T) ->
    prettypr:beside(prettypr:beside(prettypr:text("("),
                                    prettypr_term(false, T)),
                    prettypr:text(")")).

prettypr_succ({zero, _}, N) ->
    prettypr:text(integer_to_list(N));
prettypr_succ({succ, _, T}, N) ->
    prettypr_succ(T, N+1);
prettypr_succ(T, _N) ->
    prettypr:follow(prettypr:text("(succ"),
                    prettypr:beside(prettypr_a_term(false, T),
                                    prettypr:text(")")), 2).

%%. vim: foldmethod=marker foldmarker=%%',%%.
