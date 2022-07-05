-module(recon_syntax).

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
-export([term_subst_top/2,
         type_shift/2,
         type_subst_top/2]).

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

-type ty() :: {var, index(), context_size()}
            | {id, string()}
            | {arr, ty(), ty()}
            | unit
            | {record, [{string(), ty()}]}
            | {rec, string(), ty()}
            | {variant, [{string(), ty()}]}
            | bool
            | string
            | float
            | nat.

-type info() :: {pos_integer(), pos_integer()}.

-type token() :: {atom(), info()}
               | {atom(), info(), string()}.

-type index() :: non_neg_integer().
-type context_size() :: non_neg_integer().

-type term_() :: {true, info()}
               | {false, info()}
               | {if_, info(), term_(), term_(), term_()}
               | {case_, info(), term_(), [{string(), {string(), term_()}}]}
               | {tag, info(), string(), term_(), ty()}
               | {var, info(), index(), context_size()}
               | {abs, info(), string(), ty(), term_()}
               | {app, info(), term_(), term_()}
               | {let_, info(), string(), term_(), term_()}
               | {fix, info(), term_()}
               | {string, info(), string()}
               | {unit, info()}
               | {ascribe, info(), term_(), ty()}
               | {proj, info(), term_(), string()}
               | {record, info(), [{string(), term_()}]}
               | {float, info(), float()}
               | {timesfloat, info(), term_(), term_()}
               | {zero, info()}
               | {succ, info(), term_()}
               | {pred, info(), term_()}
               | {is_zero, info(), term_()}
               | {inert, info(), ty()}.
%% TAPL `term' type, but `term()' is a builtin type in Erlang,
%% hence the name `term_()'.

-type binding() :: name_bind
                 | ty_var_bind
                 | {var_bind, ty()}
                 | {tm_abb_bind, term_(), ty() | none}
                 | {ty_abb_bind, ty()}.

-type context() :: [{string(), binding()}].

-type command() :: {eval, info(), term()}
                 | {bind, info(), string(), binding()}.

%%.
%%' Constructors
%%

-spec ty(ty()) -> ty().
ty(Ty) ->
    case Ty of
        {var, _, _} -> Ty;
        {id, _} -> Ty;
        {arr, _, _} -> Ty;
        unit -> Ty;
        {record, _} -> Ty;
        {rec, _, _} -> Ty;
        {variant, _} -> Ty;
        bool -> Ty;
        string -> Ty;
        float -> Ty;
        nat -> Ty
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
        {case_, _, _, _} -> T;
        {tag, _, _, _, _} -> T;
        {var, _, _, _} -> T;
        {abs, _, _, _, _} -> T;
        {app, _, _, _} -> T;
        {let_, _, _, _, _} -> T;
        {fix, _, _} -> T;
        {string, _, _} -> T;
        {unit, _} -> T;
        {ascribe, _, _, _} -> T;
        {proj, _, _, _} -> T;
        {record, _, _} -> T;
        {float, _, _} -> T;
        {timesfloat, _, _, _} -> T;
        {zero, _} -> T;
        {succ, _, _} -> T;
        {pred, _, _} -> T;
        {is_zero, _, _} -> T;
        {inert, _, _} -> T
    end.

-spec binding(binding()) -> binding().
binding(B) ->
    case B of
        name_bind -> B;
        ty_var_bind -> B;
        {var_bind, _} -> B;
        {tm_abb_bind, _, _} -> B;
        {ty_abb_bind, _} -> B
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

-spec index_to_name(info(), context(), non_neg_integer()) -> string().
index_to_name(FInfo, Ctx, I) ->
    case Ctx of
        [] ->
            erlang:error({variable_lookup_failure, FInfo, I, context_length(Ctx)}, [FInfo, Ctx, I]);
        [_|_] ->
            %% OCaml List indexing is 0 based, Erlang lists indexing is 1 based,
            %% so I+1 instead of I.
            {X, _} = lists:nth(?assert_type(I+1, pos_integer()), ?assert_type(Ctx, [{string(), binding()}, ...])),
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

-spec type_map(OnTyVarF, _, ty()) -> ty() when
      OnTyVarF :: fun((non_neg_integer(), non_neg_integer(), non_neg_integer()) -> ty()).
type_map(OnTyVarF, C, Ty) ->
    case Ty of
        {var, X, N} -> OnTyVarF(C, X, N);
        {id, _} -> Ty;
        string -> Ty;
        unit -> Ty;
        {record, FieldTys} ->
            {record, [ {L, type_map(OnTyVarF, C, FTy)} || {L, FTy} <- FieldTys ]};
        {rec, X, TyT} -> {rec, X, type_map(OnTyVarF, C+1, TyT)};
        float -> Ty;
        bool -> Ty;
        nat -> Ty;
        {arr, Ty1, Ty2} ->
            {arr, type_map(OnTyVarF, C, Ty1), type_map(OnTyVarF, C, Ty2)};
        {variant, FieldTys} ->
            {variant, [ {L, type_map(OnTyVarF, C, FTy)} || {L, FTy} <- FieldTys ]}
    end.

-spec term_map(OnVarF, OnTypeF, _, term_()) -> term_() when
      OnVarF :: fun((info(), pos_integer(), non_neg_integer(), non_neg_integer()) -> term_()),
      OnTypeF :: fun((...) -> ty()).
term_map(OnVarF, OnTypeF, C, T) ->
    case T of
        {inert, FInfo, Ty} ->
            {inert, FInfo, OnTypeF(C, Ty)};
        {var, FInfo, X, N} ->
            OnVarF(FInfo, C, X, N);
        {abs, FInfo, X, Ty1, T2} ->
            {abs, FInfo, X, OnTypeF(C, Ty1), term_map(OnVarF, OnTypeF, C+1, T2)};
        {app, FInfo, T1, T2} ->
            {app, FInfo, term_map(OnVarF, OnTypeF, C, T1), term_map(OnVarF, OnTypeF, C, T2)};
        {let_, FInfo, X, T1, T2} ->
            {let_, FInfo, X, term_map(OnVarF, OnTypeF, C, T1), term_map(OnVarF, OnTypeF, C+1, T2)};
        {fix, FInfo, T1} ->
            {fix, FInfo, term_map(OnVarF, OnTypeF, C, T1)};
        {true, _} ->
            T;
        {false, _} ->
            T;
        {if_, FInfo, T1, T2, T3} ->
            {if_, FInfo,
             term_map(OnVarF, OnTypeF, C, T1),
             term_map(OnVarF, OnTypeF, C, T2),
             term_map(OnVarF, OnTypeF, C, T3)};
        {string, _, _} ->
            T;
        {unit, _} ->
            T;
        {proj, FInfo, T1, Label} ->
            {proj, FInfo, term_map(OnVarF, OnTypeF, C, T1), Label};
        {record, FInfo, Fields} ->
            {record, FInfo, [ {Label, term_map(OnVarF, OnTypeF, C, FieldT)}
                              || {Label, FieldT} <- Fields ]};
        {ascribe, FInfo, T1, Ty1} ->
            {ascribe, FInfo, term_map(OnVarF, OnTypeF, C, T1), OnTypeF(C, Ty1)};
        {float, _, _} ->
            T;
        {timesfloat, FInfo, T1, T2} ->
            {timesfloat, FInfo, term_map(OnVarF, OnTypeF, C, T1), term_map(OnVarF, OnTypeF, C, T2)};
        {zero, _} ->
            T;
        {succ, FInfo, T1} ->
            {succ, FInfo, term_map(OnVarF, OnTypeF, C, T1)};
        {pred, FInfo, T1} ->
            {pred, FInfo, term_map(OnVarF, OnTypeF, C, T1)};
        {is_zero, FInfo, T1} ->
            {is_zero, FInfo, term_map(OnVarF, OnTypeF, C, T1)};
        {tag, FInfo, Label, T1, Ty} ->
            {tag, FInfo, Label, term_map(OnVarF, OnTypeF, C, T1), OnTypeF(C, Ty)};
        {case_, FInfo, T1, Cases} ->
            NewCases = [ {L, {X, term_map(OnVarF, OnTypeF, C+1, Ty)}} || {L, {X, Ty}} <- Cases ],
            {case_, FInfo, term_map(OnVarF, OnTypeF, C, T1), NewCases}
    end.

-spec type_shift_above(integer(), non_neg_integer(), ty()) -> ty().
type_shift_above(D, C, Ty) ->
    type_map(fun
                 (C1, X, N) when X >= C1 -> {var, X+D, N+D};
                 ( _, X, N) -> {var, X, N+D}
             end, C, Ty).

-spec term_shift_above(integer(), non_neg_integer(), term_()) -> term_().
term_shift_above(D, C, T) ->
    term_map(fun
                 (FInfo, C1, X, N) when X >= C1 -> {var, FInfo, X+D, N+D};
                 (FInfo, _, X, N) -> {var, FInfo, X, N+D}
             end,
             fun (C1, Ty) -> type_shift_above(D, C1, Ty) end,
             C, T).

-spec term_shift(integer(), term_()) -> term_().
term_shift(D, T) ->
    term_shift_above(D, 0, T).

-spec type_shift(integer(), ty()) -> ty().
type_shift(D, Ty) -> type_shift_above(D, 0, Ty).

-spec binding_shift(_, binding()) -> binding().
binding_shift(D, Bind) ->
    case Bind of
        name_bind -> name_bind;
        ty_var_bind -> ty_var_bind;
        {tm_abb_bind, T, MaybeTy} ->
            {tm_abb_bind, term_shift(D, T), case MaybeTy of
                                                none -> none;
                                                Ty -> type_shift(D, Ty)
                                            end};
        {var_bind, Ty} ->
            {var_bind, type_shift(D, Ty)};
        {ty_abb_bind, Ty} ->
            {ty_abb_bind, type_shift(D, Ty)}
    end.

%%.
%%' Substitution
%%

term_subst(J, S, T) ->
    term_map(fun
                 (_FInfo, J1, X, _) when X == J1 -> term_shift(J1, S);
                 ( FInfo, _, X, N) -> {var, FInfo, X, N}
             end,
             fun (_J1, Ty) -> Ty end,
             J, T).

-spec term_subst_top(term_(), term_()) -> term_().
term_subst_top(S, T) ->
    term_shift(-1, term_subst(0, term_shift(1, S), T)).

type_subst(TyS, J, TyT) ->
    type_map(fun
                 (J1, X, _) when X == J1 -> type_shift(J1, TyS);
                 ( _, X, N) -> {var, X, N}
             end, J, TyT).

-spec type_subst_top(ty(), ty()) -> ty().
type_subst_top(TyS, TyT) ->
    type_shift(-1, type_subst(type_shift(1, TyS), 0, TyT)).

type_term_subst(TyS, J, T) ->
    term_map(fun (Info, _, X, N) -> {var, Info, X, N} end,
             fun (J1, TyT) -> type_subst(TyS, J1, TyT) end,
             J, T).

type_term_subst_top(TyS, T) ->
    %% gradualizer TODO:
    %% "The integer 1 is expected to have type neg_integer() but it has type 1" is printed
    %% for the next line, but it seems invalid.
    %% All the funs below and down the call chain use only non-neg integers and addition,
    %% so there's no reason for 1 to be required to be negative.
    term_shift(-1, type_term_subst(type_shift(1, TyS), 0, T)).

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
            binding_shift(I+1, Bind)
    end.

-spec get_type_from_context(info(), context(), non_neg_integer()) -> ty().
get_type_from_context(Info, Ctx, I) ->
    case get_binding(Info, Ctx, I) of
        {var_bind, Ty} -> Ty;
        {tm_abb_bind, _, none} ->
            erlang:error({no_type_recorded_for_variable, index_to_name(Info, Ctx, I)},
                         [Info, Ctx, I]);
        {tm_abb_bind, _, Ty} ->
            Ty;
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
        {true, Info} -> Info;
        {false, Info} -> Info;
        {if_, Info, _, _, _} -> Info;
        {case_, Info, _, _} -> Info;
        {tag, Info, _, _, _} -> Info;
        {var, Info, _, _} -> Info;
        {abs, Info, _, _, _} -> Info;
        {app, Info, _, _} -> Info;
        {let_, Info, _, _, _} -> Info;
        {fix, Info, _} -> Info;
        {string, Info, _} -> Info;
        {unit, Info} -> Info;
        {ascribe, Info, _, _} -> Info;
        {proj, Info, _, _} -> Info;
        {record, Info, _} -> Info;
        {float, Info, _} -> Info;
        {timesfloat, Info, _, _} -> Info;
        {zero, Info} -> Info;
        {succ, Info, _} -> Info;
        {pred, Info, _} -> Info;
        {is_zero, Info, _} -> Info;
        {inert, Info, _} -> Info
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

format_doc(Doc, Opts) ->
    prettypr:format(Doc,
                    maps:get(paper_width, Opts, 80),
                    maps:get(line_width, Opts, 65)).

-spec prettypr_type(context(), ty()) -> prettypr:document().
prettypr_type(Ctx, Ty) ->
    prettypr_type(true, Ctx, Ty).

-spec prettypr_type(boolean(), context(), ty()) -> prettypr:document().
prettypr_type(Outer, Ctx, Ty) ->
    case Ty of
        {rec, X, TyT} ->
            {NewCtx, X2} = pick_fresh_name(Ctx, X),
            prettypr:par([prettypr:text("Rec"),
                          prettypr:beside(prettypr:text(X2),
                                          prettypr:text(".")),
                          prettypr_type(false, NewCtx, TyT)], 2);
        _ ->
            prettypr_arrow_type(Outer, Ctx, Ty)
    end.

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
        {var, X, N} ->
            case context_length(Ctx) of
                N ->
                    DummyInfo = {1,1},
                    prettypr:text(index_to_name(DummyInfo, Ctx, X));
                _ ->
                    BadIndex = ?assert_type(io_lib:format("[bad index: ~p / ~p in ~p]",
                                                          [X, N, Ctx]), string()),
                    prettypr:text(BadIndex)
            end;
        {id, B} ->
            prettypr:text(B);
        bool ->
            prettypr:text("Bool");
        {variant, []} ->
            prettypr:text("<>");
        {variant, Fields} ->
            NFields = length(Fields),
            FieldsD = [ prettypr:par([prettypr:beside(prettypr:text(Label),
                                                      prettypr:text(":")),
                                      if
                                          I /= NFields ->
                                              prettypr:beside(prettypr_type(false, Ctx, Ty1),
                                                              prettypr:text(","));
                                          I == NFields ->
                                              prettypr_type(false, Ctx, Ty1)
                                      end], 2)
                        || {I, {Label, Ty1}} <- lists:zip(lists:seq(1, NFields), Fields) ],
            prettypr:beside(prettypr:beside(prettypr:text("<"),
                                            prettypr:par(FieldsD)),
                            prettypr:text(">"));
        string ->
            prettypr:text("String");
        unit ->
            prettypr:text("Unit");
        {record, []} ->
            prettypr:text("{}");
        {record, Fields} ->
            FieldsD = lists:join(prettypr:text(","),
                                 [ prettypr:par([prettypr:text(Label),
                                                 prettypr:text(":"),
                                                 prettypr_type(false, Ctx, Ty1)], 2)
                                   || {Label, Ty1} <- Fields ]),
            prettypr:par([prettypr:text("{")] ++ FieldsD ++ [prettypr:text("}")], 2);
        float ->
            prettypr:text("Float");
        nat ->
            prettypr:text("Nat");
        _ ->
            prettypr:par([prettypr:text("("),
                          prettypr_type(Outer, Ctx, Ty),
                          prettypr:text(")")], 2)
    end.

-spec prettypr_term(context(), term_()) -> prettypr:document().
prettypr_term(Ctx, T) ->
    prettypr_term(true, Ctx, T).

-spec prettypr_term(boolean(), context(), term_()) -> prettypr:document().
prettypr_term(Outer, Ctx, T) ->
    case T of
        {if_, _Info, T1, T2, T3} ->
            prettypr:sep([
                          prettypr:par([prettypr:text("if"),
                                        prettypr_term(false, Ctx, T1)], 2),
                          prettypr:par([prettypr:text("then"),
                                        prettypr_term(false, Ctx, T2)], 2),
                          prettypr:par([prettypr:text("else"),
                                        prettypr_term(false, Ctx, T3)], 2)
                         ]);
        {case_, _Info, T1, Cases} ->
            CaseDocs = lists:map(fun ({Label, {X, Ty1}}) ->
                                         {NewCtx, X_} = pick_fresh_name(Ctx, X),
                                         prettypr:par([prettypr:text("<"),
                                                       prettypr:text(Label),
                                                       prettypr:text("="),
                                                       prettypr:text(X_),
                                                       prettypr:text(">==>"),
                                                       prettypr_term(false, NewCtx, Ty1)], 2)
                                 end, Cases),
            prettypr:par([prettypr:text("case"),
                          prettypr_term(false, Ctx, T1),
                          prettypr:text("of")] ++ CaseDocs, 0);
        {abs, _Info, X, Ty1, T2} ->
            {NewCtx, X2} = pick_fresh_name(Ctx, X),
            prettypr:par([prettypr:text("lambda"),
                          prettypr:beside(prettypr:text(X2),
                                          prettypr:text(":")),
                          prettypr:beside(prettypr_type(false, Ctx, Ty1),
                                          prettypr:text(".")),
                          prettypr_term(Outer, NewCtx, T2)], 2);
        {let_, _Info, X, T1, T2} ->
            prettypr:par([prettypr:text(string:join(["let ", X, " = "], "")),
                          prettypr:beside(prettypr_term(false, Ctx, T1),
                                          prettypr:text("in")),
                          prettypr_term(false, add_name(Ctx, X), T2)], 0);
        {fix, _Info, T1} ->
            prettypr:follow(prettypr:text("fix"),
                            prettypr_term(false, Ctx, T1), 2);
        _ ->
            prettypr_app_term(Outer, Ctx, T)
    end.

-spec prettypr_app_term(boolean(), context(), term_()) -> prettypr:document().
prettypr_app_term(Outer, Ctx, T) ->
    case T of
        {app, _Info, T1, T2} ->
            prettypr:par([prettypr_app_term(false, Ctx, T1),
                          prettypr_a_term(false, Ctx, T2)], 0);
        {timesfloat, _Info, T1, T2} ->
            prettypr:follow(prettypr:follow(prettypr:text("timesfloat"),
                                            prettypr_a_term(false, Ctx, T1)),
                            prettypr_a_term(false, Ctx, T2));
        {pred, _Info, T} ->
            prettypr:follow(prettypr:text("pred"),
                            prettypr_a_term(false, Ctx, T));
        {is_zero, _Info, T} ->
            prettypr:follow(prettypr:text("iszero"),
                            prettypr_a_term(false, Ctx, T));
        _ ->
            prettypr_path_term(Outer, Ctx, T)
    end.

-spec prettypr_ascribe_term(boolean(), context(), term_()) -> prettypr:document().
prettypr_ascribe_term(Outer, Ctx, T) ->
    case T of
        {ascribe, _Info, T1, Ty1} ->
            prettypr:par([prettypr_app_term(false, Ctx, T1),
                          prettypr:text("as"),
                          prettypr_type(false, Ctx, Ty1)], 2);
        _ ->
            prettypr_a_term(Outer, Ctx, T)
    end.

-spec prettypr_path_term(boolean(), context(), term_()) -> prettypr:document().
prettypr_path_term(Outer, Ctx, T) ->
    case T of
        {proj, _Info, T1, Label} ->
            prettypr:beside(prettypr:beside(prettypr_a_term(false, Ctx, T1), prettypr:text(".")),
                            prettypr:text(Label));
        _ ->
            prettypr_ascribe_term(Outer, Ctx, T)
    end.

-spec prettypr_a_term(boolean(), context(), term_()) -> prettypr:document().
prettypr_a_term(Outer, Ctx, T) ->
    case T of
        {inert, _Info, Ty} ->
            prettypr:par([prettypr:text("inert["),
                          prettypr_type(false, Ctx, Ty),
                          prettypr:text("]")], 2);
        {true, _} ->
            prettypr:text("true");
        {false, _} ->
            prettypr:text("false");
        {tag, _, Label, T1, Ty} ->
            prettypr:par([prettypr:text("<"),
                          prettypr:text(Label),
                          prettypr:text("="),
                          prettypr_term(false, Ctx, T1),
                          prettypr:text(">"),
                          prettypr:text("as"),
                          prettypr_type(Outer, Ctx, Ty)], 2);
        {var, Info, X, N} ->
            case context_length(Ctx) of
                N ->
                    prettypr:text(index_to_name(Info, Ctx, X));
                _ ->
                    prettypr:text(io_lib:format("[bad index: ~p / ~p in ~p]", [X, N, Ctx]))
            end;
        {string, _Info, S} ->
            prettypr:text(io_lib:format("~p", [S]));
        {unit, _} ->
            prettypr:text("unit");
        {record, _Info, Fields} ->
            FieldsD = lists:join(prettypr:text(","),
                                 [ prettypr:par([prettypr:text(Label),
                                                 prettypr:text("="),
                                                 prettypr_term(false, Ctx, T1)], 2)
                                   || {Label, T1} <- Fields ]),
            prettypr:par([prettypr:text("{")] ++ FieldsD ++ [prettypr:text("}")], 2);
        {float, _Info, S} ->
            prettypr:text(io_lib:format("~p", [S]));
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
        ty_var_bind ->
            prettypr:empty();
        {var_bind, Ty} ->
            prettypr:follow(prettypr:text(":"),
                            prettypr_type(Ctx, Ty), 2);
        {tm_abb_bind, T, _Ty} ->
            prettypr:follow(prettypr:text("="),
                            prettypr_term(Ctx, T), 2);
        {ty_abb_bind, Ty} ->
            prettypr:follow(prettypr:text("="),
                            prettypr_type(Ctx, Ty), 2)
    end.

%%.
%%' Tests
%%

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

format_term_test_() ->
    [
     ?_test(format_term( [], {true, 0}                                          )),
     ?_test(format_term( [], {false, 0}                                         )),
     ?_test(format_term( [], {if_, 0, {is_zero, 0, {zero, 0}},
                              {zero, 0},
                              {succ, 0, {zero, 0}}}                             )),
     ?_test(format_term( [], {var, 0, 1, 1}                                     )),
     ?_test(format_term( [], {abs, 0, "x", {true, 0}}                           )),
     ?_test(format_term( [], {app, 0, {true, 0}, {false, 0}}                    )),
     ?_test(format_term( [], {proj, 0, {true, 0}, "x"}                          )),
     ?_test(format_term( [], {record, 0, [{"a", {true, 0}}]}                    )),
     ?_test(format_term( [], {float, 0, 3.14}                                   )),
     ?_test(format_term( [], {times_float, 0, {float, 0, 2.0}, {float, 0, 3.0}} )),
     ?_test(format_term( [], {string, 0, "ala ma kota, a kot ma ale"}           )),
     ?_test(format_term( [], {zero, 0}                                          )),
     ?_test(format_term( [], {succ, 0, {zero, 0}}                               )),
     ?_test(format_term( [], {pred, 0, {succ, 0, {zero, 0}}}                    )),
     ?_test(format_term( [], {is_zero, 0, {zero, 0}}                            )),
     ?_test(format_term( [], {let_, 0, "a", {true, 0}, {false, 0}}              ))
    ].

format_binding_test_() ->
    [
     ?_test(format_binding( empty_context(), name_bind                      )),
     ?_test(format_binding( add_name(empty_context(), "x"),
                            {abb_bind, {var, 0, 1, 1}}                      ))
    ].

-endif. %% TEST

%%. vim: foldmethod=marker foldmarker=%%',%%.
