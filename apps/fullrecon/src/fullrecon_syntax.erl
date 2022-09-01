-module(fullrecon_syntax).

%% Constructors
-export([ty/1,
         binding/1,
         command/1,
         info/1,
         term_/1]).

%% Context management
-export([add_binding/3,
         add_name/2,
         context_length/1,
         empty_context/0,
         get_binding/3,
         get_type_from_context/3,
         is_name_bound/2,
         name_to_index/3]).

%% Substitution
-export([term_subst_top/2]).

%% Printing
-export([format_binding/2, format_binding/3,
         format_doc/2,
         format_term/2, format_term/3,
         format_type/2, format_type/3,
         prettypr_a_term/3]).

-export([term_info/1]).

-export_type([binding/0,
              command/0,
              context/0,
              info/0,
              term_/0,
              ty/0]).

-include_lib("gradualizer/include/gradualizer.hrl").

%%
%%' Datatypes
%%

-type ty() :: bool
            | nat
            | {arr, ty(), ty()}
            | {id, string()}.

-type info() :: {integer(), integer()}.

-type token() :: {atom(), info()}
               | {atom(), info(), string()}.

-type index() :: non_neg_integer().
-type context_size() :: non_neg_integer().

-type maybe(T) :: none | {ok, T}.

-type term_() :: {var, info(), index(), context_size()}
               | {let_, info(), string(), term_(), term_()}
               | {true, info()}
               | {false, info()}
               | {if_, info(), term_(), term_(), term_()}
               | {zero, info()}
               | {succ, info(), term_()}
               | {pred, info(), term_()}
               | {is_zero, info(), term_()}
               | {abs, info(), string(), maybe(ty()), term_()}
               | {app, info(), term_(), term_()}.
%% TAPL `term' type, but `term()' is a builtin type in Erlang,
%% hence the name `term_()'.

-type binding() :: name_bind
                 | {var_bind, ty()}.

-type context() :: [{string(), binding()}].

-type command() :: {eval, info(), term_()}
                 | {bind, info(), string(), binding()}.

%%.
%%' Constructors
%%

-spec ty(ty()) -> ty().
ty(Ty) ->
    case Ty of
        bool -> Ty;
        nat -> Ty;
        {id, _} -> Ty;
        {arr, _, _} -> Ty
    end.

-spec info(token()) -> info().
info({_, Info}) -> Info;
info({_, Info, _Chars}) -> Info.

%% This function might seem useless, since it "doesn't do anything",
%% but in fact it gives us two guarantees:
%% - thanks to Gradualizer it provides compile-time warnings if we build an invalid term_()
%% - it fails fast (aka crashes) at runtime if we try to build an invalid term_()
-spec term_(term_()) -> term_().
term_(T) ->
    case T of
        {true, _} -> T;
        {false, _} -> T;
        {if_, _, _, _, _} -> T;
        {var, _, _, _} -> T;
        {abs, _, _, _, _} -> T;
        {app, _, _, _} -> T;
        {let_, _, _, _, _} -> T;
        {zero, _} -> T;
        {succ, _, _} -> T;
        {pred, _, _} -> T;
        {is_zero, _, _} -> T
    end.

-spec binding(binding()) -> binding().
binding(B) ->
    case B of
        name_bind -> B;
        {var_bind, _} -> B
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
            X1 = case re:run(X, "([a-zA-Z]+)([0-9]+)", [{capture, all_but_first, list}]) of
                     {match, [AlNum, Seq]} ->
                         AlNum ++ integer_to_list(list_to_integer(Seq) + 1);
                     nomatch ->
                         X ++ "1"
                 end,
            {add_name(Ctx, X1), X1}
    end.

-spec index_to_name(info(), context(), integer()) -> string().
index_to_name(FInfo, Ctx, I) ->
    case Ctx of
        [] ->
            erlang:error({variable_lookup_failure, FInfo, I, context_length(Ctx)}, [FInfo, Ctx, I]);
        [_|_] ->
            Ctx = ?assert_type(Ctx, [T, ...]),
            %% OCaml List indexing is 0 based, Erlang lists indexing is 1 based,
            %% so I+1 instead of I.
            {X, _} = lists:nth(?assert_type(I+1, pos_integer()), Ctx),
            X
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

-spec term_map(OnVarF, non_neg_integer(), term_()) -> term_() when
      OnVarF :: fun((info(), non_neg_integer(), index(), context_size()) -> term_()).
term_map(OnVarF, C, T) ->
    case T of
        {var, FInfo, X, N} ->
            OnVarF(FInfo, C, X, N);
        {let_, FInfo, X, T1, T2} ->
            {let_, FInfo, X, term_map(OnVarF, C, T1), term_map(OnVarF, C+1, T2)};
        {true, _} ->
            T;
        {false, _} ->
            T;
        {if_, FInfo, T1, T2, T3} ->
            {if_, FInfo,
             term_map(OnVarF, C, T1),
             term_map(OnVarF, C, T2),
             term_map(OnVarF, C, T3)};
        {zero, _} ->
            T;
        {succ, FInfo, T1} ->
            {succ, FInfo, term_map(OnVarF, C, T1)};
        {pred, FInfo, T1} ->
            {pred, FInfo, term_map(OnVarF, C, T1)};
        {is_zero, FInfo, T1} ->
            {is_zero, FInfo, term_map(OnVarF, C, T1)};
        {abs, FInfo, X, Ty1, T2} ->
            {abs, FInfo, X, Ty1, term_map(OnVarF, C+1, T2)};
        {app, FInfo, T1, T2} ->
            {app, FInfo, term_map(OnVarF, C, T1), term_map(OnVarF, C, T2)}
    end.

-spec term_shift_above(integer(), non_neg_integer(), term_()) -> term_().
term_shift_above(D, C, T) ->
    term_map(fun
                 (FInfo, C1, X, N) when X >= C1 ->
                     NewX = ?assert_type(X + D, non_neg_integer()),
                     NewX < 0 andalso erlang:error(negative_context_index),
                     NewN = ?assert_type(N + D, non_neg_integer()),
                     NewN < 0 andalso erlang:error(negative_context_size),
                     {var, FInfo, NewX, NewN};
                 (FInfo, _, X, N) ->
                     NewN = ?assert_type(N + D, non_neg_integer()),
                     NewN < 0 andalso erlang:error(negative_context_size),
                     {var, FInfo, X, NewN}
             end,
             C, T).

-spec term_shift(integer(), term_()) -> term_().
term_shift(D, T) ->
    term_shift_above(D, 0, T).

%%.
%%' Substitution
%%

term_subst(J, S, T) ->
    term_map(fun
                 (_FInfo, J1, X, _) when X == J1 -> term_shift(J1, S);
                 ( FInfo, _, X, N) -> {var, FInfo, X, N}
             end,
             J, T).

-spec term_subst_top(term_(), term_()) -> term_().
term_subst_top(S, T) ->
    term_shift(-1, term_subst(0, term_shift(1, S), T)).

%%.
%%' Context management (continued)
%%

-spec get_binding(info(), context(), index()) -> binding().
get_binding(FInfo, Ctx, I) ->
    case Ctx of
        [] ->
            erlang:error({variable_lookup_failure, FInfo, I, context_length(Ctx)}, [FInfo, Ctx, I]);
        [_|_] ->
            %% OCaml List indexing is 0 based, Erlang lists indexing is 1 based,
            %% so I+1 instead of I.
            {_, Bind} = lists:nth(?assert_type(I+1, pos_integer()), ?assert_type(Ctx, [T, ...])),
            Bind
    end.

-spec get_type_from_context(info(), context(), non_neg_integer()) -> ty().
get_type_from_context(Info, Ctx, I) ->
    case get_binding(Info, Ctx, I) of
        {var_bind, Ty} -> Ty;
        _ ->
            erlang:error({wrong_kind_of_binding_for_variable, index_to_name(Info, Ctx, I)},
                         [Info, Ctx, I])
    end.

%%.
%%' Extracting file info
%%

-spec term_info(term_()) -> info().
term_info(T) ->
    case T of
        {var, Info, _, _} -> Info;
        {let_, Info, _, _, _} -> Info;
        {true, Info} -> Info;
        {false, Info} -> Info;
        {if_, Info, _, _, _} -> Info;
        {zero, Info} -> Info;
        {succ, Info, _} -> Info;
        {pred, Info, _} -> Info;
        {is_zero, Info, _} -> Info;
        {abs, Info, _, _, _} -> Info;
        {app, Info, _, _} -> Info
    end.

%%.
%%' Printing
%%

-spec format_term(context(), term_()) -> string().
format_term(Ctx, T) -> format_term(Ctx, T, #{}).

-spec format_term(context(), term_(), map()) -> string().
format_term(Ctx, T, Opts) ->
    Doc = prettypr_term(true, Ctx, T),
    format_doc(Doc, Opts).

-spec format_type(context(), ty()) -> string().
format_type(Ctx, T) -> format_type(Ctx, T, #{}).

-spec format_type(context(), ty(), map()) -> string().
format_type(Ctx, T, Opts) ->
    Doc = prettypr_type(true, Ctx, T),
    format_doc(Doc, Opts).

-spec format_doc(prettypr:document(), map()) -> string().
format_doc(Doc, Opts) ->
    prettypr:format(Doc,
                    maps:get(paper_width, Opts, 80),
                    maps:get(line_width, Opts, 65)).

-spec prettypr_type(context(), ty()) -> prettypr:document().
prettypr_type(Ctx, Ty) ->
    prettypr_type(true, Ctx, Ty).

-spec prettypr_type(boolean(), context(), ty()) -> prettypr:document().
prettypr_type(Outer, Ctx, Ty) ->
    prettypr_arrow_type(Outer, Ctx, Ty).

-spec prettypr_arrow_type(boolean(), context(), ty()) -> prettypr:document().
prettypr_arrow_type(Outer, Ctx, Ty) ->
    case Ty of
        {arr, Ty1, Ty2} ->
            prettypr:par([prettypr_a_type(false, Ctx, Ty1),
                          prettypr:text("->"),
                          prettypr_a_type(false, Ctx, Ty2)], 2);
        _ ->
            prettypr_a_type(Outer, Ctx, Ty)
    end.

-spec prettypr_a_type(boolean(), context(), ty()) -> prettypr:document().
prettypr_a_type(Outer, Ctx, Ty) ->
    case Ty of
        bool ->
            prettypr:text("Bool");
        nat ->
            prettypr:text("Nat");
        {id, B} ->
            prettypr:text(B);
        _ ->
            prettypr:par([prettypr:text("("),
                          prettypr_type(Outer, Ctx, Ty),
                          prettypr:text(")")], 2)
    end.

-spec prettypr_term(boolean(), context(), term_()) -> prettypr:document().
prettypr_term(Outer, Ctx, T) ->
    case T of
        {let_, _Info, X, T1, T2} ->
            prettypr:par([prettypr:text(string:join(["let ", X, " = "], "")),
                          prettypr:beside(prettypr_term(false, Ctx, T1),
                                          prettypr:text("in")),
                          prettypr_term(false, add_name(Ctx, X), T2)], 0);
        {if_, _Info, T1, T2, T3} ->
            prettypr:sep([
                          prettypr:par([prettypr:text("if"),
                                        prettypr_term(false, Ctx, T1)], 2),
                          prettypr:par([prettypr:text("then"),
                                        prettypr_term(false, Ctx, T2)], 2),
                          prettypr:par([prettypr:text("else"),
                                        prettypr_term(false, Ctx, T3)], 2)
                         ]);
        {abs, _Info, X, {ok, Ty1}, T2} ->
            {NewCtx, X2} = pick_fresh_name(Ctx, X),
            prettypr:par([prettypr:text("lambda"),
                          prettypr:beside(prettypr:text(X2),
                                          prettypr:text(":")),
                          prettypr:beside(prettypr_type(false, Ctx, Ty1),
                                          prettypr:text(".")),
                          prettypr_term(Outer, NewCtx, T2)], 2);
        {abs, _Info, X, none, T2} ->
            {NewCtx, X2} = pick_fresh_name(Ctx, X),
            prettypr:par([prettypr:text("lambda"),
                          prettypr:beside(prettypr:text(X2),
                                          prettypr:text(".")),
                          prettypr_term(Outer, NewCtx, T2)], 2);
        _ ->
            prettypr_app_term(Outer, Ctx, T)
    end.

-spec prettypr_app_term(boolean(), context(), term_()) -> prettypr:document().
prettypr_app_term(Outer, Ctx, T) ->
    case T of
        {pred, _Info, T} ->
            prettypr:follow(prettypr:text("pred"),
                            prettypr_a_term(false, Ctx, T));
        {is_zero, _Info, T} ->
            prettypr:follow(prettypr:text("iszero"),
                            prettypr_a_term(false, Ctx, T));
        {app, _Info, T1, T2} ->
            prettypr:par([prettypr_app_term(false, Ctx, T1),
                          prettypr_a_term(false, Ctx, T2)], 0);
        _ ->
            prettypr_a_term(Outer, Ctx, T)
    end.

-define(chars_to_string(Chars),
        binary_to_list(iolist_to_binary(?assert_type(Chars, list())))).

-spec prettypr_a_term(boolean(), context(), term_()) -> prettypr:document().
prettypr_a_term(_Outer, Ctx, T) ->
    case T of
        {var, Info, X, N} ->
            case context_length(Ctx) of
                N ->
                    prettypr:text(index_to_name(Info, Ctx, X));
                _ ->
                    Msg = ?chars_to_string(io_lib:format("[bad index: ~p / ~p in ~p]", [X, N, Ctx])),
                    prettypr:text(Msg)
            end;
        {true, _} ->
            prettypr:text("true");
        {false, _} ->
            prettypr:text("false");
        {zero, _} ->
            prettypr:text("0");
        {succ, _, T1} ->
            prettypr_succ(Ctx, T1, 1);
        _ ->
            prettypr:beside(prettypr:beside(prettypr:text("("),
                                            prettypr_term(false, Ctx, T)),
                            prettypr:text(")"))
    end.

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
    Doc = prettypr_binding(Ctx, B),
    prettypr:format(Doc,
                    maps:get(paper_width, Opts, 80),
                    maps:get(line_width, Opts, 65)).

-spec prettypr_binding(context(), binding()) -> prettypr:document().
prettypr_binding(Ctx, B) ->
    case B of
        name_bind ->
            prettypr:empty();
        {var_bind, Ty} ->
            prettypr:follow(prettypr:text(":"),
                            prettypr_type(Ctx, Ty), 2)
    end.

%%. vim: foldmethod=marker foldmarker=%%',%%.
