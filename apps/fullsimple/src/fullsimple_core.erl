%% @doc This module implements TAPL chapters 5, 6, 7.
%% See https://www.cis.upenn.edu/~bcpierce/tapl/ for the book.
-module(fullsimple_core).

-export([eval/2,
         eval_binding/2,
         type_of/2,
         types_equiv/3,
         type_error/2, type_error/3]).

-define(syntax, fullsimple_syntax).

-type binding() :: ?syntax:binding().
-type context() :: ?syntax:context().
-type index()   :: ?syntax:index().
-type info()    :: ?syntax:info().
-type term_()   :: ?syntax:term_().
-type ty()      :: ?syntax:ty().

-include_lib("gradualizer/include/gradualizer.hrl").

-spec eval(context(), term_()) -> term_().
eval(Ctx, T) ->
    try
        eval(Ctx, eval1(Ctx, T))
    catch throw:no_rule_applies ->
        T
    end.

-spec eval_binding(context(), binding()) -> binding().
eval_binding(Ctx, B) ->
    case B of
        {tm_abb_bind, T, Ty} ->
            T_ = eval(Ctx, T),
            {tm_abb_bind, T_, Ty};
        Bind ->
            Bind
    end.

-spec eval1(context(), term_()) -> term_().
eval1(Ctx, T) ->
    case T of
        {if_, _, {true, _}, T2, _} ->
            T2;
        {if_, _, {false, _}, _, T3} ->
            T3;
        {if_, Info, T1, T2, T3} ->
            T1_ = eval1(Ctx, T1),
            {if_, Info, T1_, T2, T3};
        {tag, Info, L, T1, Ty} ->
            T1_ = eval1(Ctx, T1),
            {tag, Info, L, T1_, Ty};
        {case_, Info, T1, Branches} ->
            case T1 of
                {tag, _, L, V, _} ->
                    case is_value(Ctx, V) of
                        true ->
                            case lists:keyfind(L, 1, Branches) of
                                {_, {_, Body}} ->
                                    ?syntax:term_subst_top(V, Body);
                                _ ->
                                    throw(no_rule_applies)
                            end;
                        false ->
                            T1_ = eval1(Ctx, T1),
                            {case_, Info, T1_, Branches}
                    end;
                _ ->
                    T1_ = eval1(Ctx, T1),
                    {case_, Info, T1_, Branches}
            end;
        {app, Info, T1, T2} ->
            T1IsVal = is_value(Ctx, T1),
            T2IsVal = is_value(Ctx, T2),
            case {T1, T1IsVal, T2IsVal} of
                {{abs, _, _, _, T12}, _, true} ->
                    ?syntax:term_subst_top(T2, T12);
                {_, true, _} ->
                    T2_ = eval1(Ctx, T2),
                    {app, Info, T1, T2_};
                _ ->
                    T1_ = eval1(Ctx, T1),
                    {app, Info, T1_, T2}
            end;
        {let_, Info, X, T1, T2} ->
            case is_value(Ctx, T1) of
                true ->
                    ?syntax:term_subst_top(T1, T2);
                false ->
                    T1_ = eval1(Ctx, T1),
                    {let_, Info, X, T1_, T2}
            end;
        {fix, Info, T1} ->
            case {T1, is_value(Ctx, T1)} of
                {{abs, _, _, _, T12}, true} ->
                    ?syntax:term_subst_top(T, T12);
                {_, true} ->
                    throw(no_rule_applies);
                _ ->
                    T1_ = eval1(Ctx, T1),
                    {fix, Info, T1_}
            end;
        {var, Info, Index, _} ->
            case ?syntax:get_binding(Info, Ctx, Index) of
                {tm_abb_bind, T1, _} ->
                    T1;
                _ ->
                    throw(no_rule_applies)
            end;
        {ascribe, Info, T1, Ty1} ->
            case is_value(Ctx, T1) of
                true ->
                    T1;
                false ->
                    T1_ = eval1(Ctx, T1),
                    {ascribe, Info, T1_, Ty1}
            end;
        {record, Info, Fields} ->
            Fields_ = eval_fields(Ctx, Fields),
            {record, Info, Fields_};
        {proj, Info, T1, Label} ->
            case {T1, is_value(Ctx, T1)} of
                {{record, _, Fields}, true} ->
                    case lists:keyfind(Label, 1, Fields) of
                        false -> throw(no_rule_applies);
                        {_, Val} -> Val
                    end;
                {_, false} ->
                    T1_ = eval1(Ctx, T1),
                    {proj, Info, T1_, Label}
            end;
        {timesfloat, Info, {float, _, F1}, {float, _, F2}} ->
            {float, Info, F1 * F2};
        {timesfloat, Info, {float, _, _} = T1, T2} ->
            T2_ = eval1(Ctx, T2),
            {timesfloat, Info, T1, T2_};
        {timesfloat, Info, T1, T2} ->
            T1_ = eval1(Ctx, T1),
            {timesfloat, Info, T1_, T2};
        {succ, Info, T1} ->
            T1_ = eval1(Ctx, T1),
            {succ, Info, T1_};
        {pred, _, {zero, _}} ->
            {zero, _DummyInfo = {1,1}};
        {pred, Info, {succ, _, NV1} = T1} ->
            NV1IsNVal = is_numeric_value(Ctx, NV1),
            case NV1IsNVal of
                true -> NV1;
                false ->
                    T1_ = eval1(Ctx, T1),
                    {pred, Info, T1_}
            end;
        {is_zero, _, {zero, _}} ->
            {true, _DummyInfo = {1,1}};
        {is_zero, _, {succ, _, _}} ->
            {false, _DummyInfo = {1,1}};
        {is_zero, Info, T1} ->
            T1_ = eval1(Ctx, T1),
            {is_zero, Info, T1_};
        _ ->
            throw(no_rule_applies)
    end.

-spec eval_fields(context(), [{string(), term_()}]) -> [{string(), term_()}].
eval_fields(_Ctx, []) ->
    throw(no_rule_applies);
eval_fields(Ctx, [{Label, T} | Fields]) ->
    case is_value(Ctx, T) of
        true ->
            Fields_ = eval_fields(Ctx, Fields),
            [{Label, T} | Fields_];
        false ->
            T_ = eval1(Ctx, T),
            [{Label, T_} | Fields]
    end.

-spec is_value(context(), term_()) -> boolean().
is_value(Ctx, T) ->
    case T of
        {true, _} -> true;
        {false, _} -> true;
        {tag, _, _, T1, _} -> is_value(Ctx, T1);
        {string, _, _} -> true;
        {float, _, _} -> true;
        {unit, _} -> true;
        {abs, _, _, _, _} -> true;
        {record, _, Fields} ->
            lists:all(fun ({_Label, T1}) -> is_value(Ctx, T1) end, Fields);
        _ ->
            is_numeric_value(Ctx, T)
    end.

-spec is_numeric_value(context(), term_()) -> boolean().
is_numeric_value(Ctx, T) ->
    case T of
        {zero, _} -> true;
        {succ, _, T1} -> is_numeric_value(Ctx, T1);
        _ -> false
    end.

-spec is_ty_abb(context(), index()) -> boolean().
is_ty_abb(Ctx, Index) ->
    case ?syntax:get_binding(_DummyInfo = {1,1}, Ctx, Index) of
        {ty_abb_bind, _} -> true;
        _ -> false
    end.

-spec get_ty_abb(context(), index()) -> ty().
get_ty_abb(Ctx, Index) ->
    case ?syntax:get_binding(_DummyInfo = {1,1}, Ctx, Index) of
        {ty_abb_bind, Ty} -> Ty;
        _ -> throw(no_rule_applies)
    end.

-spec compute_type(context(), ty()) -> ty().
compute_type(Ctx, Ty) ->
    case Ty of
        {var, Index, _} ->
            case is_ty_abb(Ctx, Index) of
                true -> get_ty_abb(Ctx, Index);
                false -> throw(no_rule_applies)
            end;
        _ ->
            throw(no_rule_applies)
    end.

-spec simplify_type(context(), ty()) -> ty().
simplify_type(Ctx, Ty) ->
    try
        Ty_ = compute_type(Ctx, Ty),
        simplify_type(Ctx, Ty_)
    catch throw:no_rule_applies ->
        Ty
    end.

-spec types_equiv(context(), ty(), ty()) -> boolean().
types_equiv(Ctx, TyS0, TyT0) ->
    TyS = simplify_type(Ctx, TyS0),
    TyT = simplify_type(Ctx, TyT0),
    case {TyS, TyT} of
        {string, string} -> true;
        {unit, unit} -> true;
        {{id, IdS}, {id, IdT}} -> IdS == IdT;
        {float, float} -> true;
        {{var, IndexS, _}, {var, IndexT, _}} ->
            case {is_ty_abb(Ctx, IndexS), is_ty_abb(Ctx, IndexT)} of
                {true, _} ->
                    types_equiv(Ctx, get_ty_abb(Ctx, IndexS), TyT);
                {_, true} ->
                    types_equiv(Ctx, TyS, get_ty_abb(Ctx, IndexT));
                _ ->
                    IndexS == IndexT
            end;
        {{arr, TyS1, TyS2}, {arr, TyT1, TyT2}} ->
            types_equiv(Ctx, TyS1, TyT1)
            andalso
            types_equiv(Ctx, TyS2, TyT2);
        {bool, bool} -> true;
        {nat, nat} -> true;
        {{record, Fields1}, {record, Fields2}} ->
            length(Fields1) == length(Fields2)
            andalso
            lists:all(fun ({Label2, Ty2}) ->
                        case lists:keyfind(Label2, 1, Fields1) of
                            {Label2, Ty1} ->
                                types_equiv(Ctx, Ty1, Ty2);
                            false ->
                                false
                        end
                      end,
                      Fields2);
        {{variant, Fields1}, {variant, Fields2}} ->
            length(Fields1) == length(Fields2)
            andalso
            lists:all(fun ({{L1, Ty1}, {L2, Ty2}}) ->
                        L1 == L2 andalso types_equiv(Ctx, Ty1, Ty2)
                      end,
                      lists:zip(Fields1, Fields2));
        _ ->
            false
    end.

-spec type_of(context(), term_()) -> ty().
type_of(Ctx, T) ->
    case T of
        {inert, _, Ty} -> Ty;
        {true, _} -> bool;
        {false, _} -> bool;
        {if_, Info, T1, T2, T3} ->
            case types_equiv(Ctx, type_of(Ctx, T1), bool) of
                true ->
                    TyT2 = type_of(Ctx, T2),
                    case types_equiv(Ctx, TyT2, type_of(Ctx, T3)) of
                        true ->
                            TyT2;
                        false ->
                            type_error(Info, "arms of conditional have different types")
                    end;
                false ->
                    type_error(Info, "guard of conditional not a boolean")
            end;
        {case_, Info, T1, Cases} ->
            case simplify_type(Ctx, type_of(Ctx, T1)) of
                {variant, FieldTys} ->
                    %% TODO: This foreach seems() quite redundant with the following map().
                    %%       Why is it done like that in the OCaml original?...
                    lists:foreach(fun ({Li, _}) ->
                                    case lists:keymember(Li, 1, FieldTys) of
                                        true ->
                                            ok;
                                        false ->
                                            type_error(Info, "label ~p not in type", [Li])
                                    end
                                  end,
                                  Cases),
                    CaseTypes = lists:map(fun ({Li, {Xi, Ti}}) ->
                                            TyTi = case lists:keyfind(Li, 1, FieldTys) of
                                                       {Li, Ty} ->
                                                           Ty;
                                                       false ->
                                                           type_error(Info, "label ~p not found", [Li])
                                                   end,
                                            NewCtx = ?syntax:add_binding(Ctx, Xi, {var_bind, TyTi}),
                                            ?syntax:type_shift(-1, type_of(NewCtx, Ti))
                                          end,
                                          Cases),
                    %% Empty Cases are not accepted by the parser,
                    %% so CaseTypes cannot be empty.
                    NECaseTypes = ?assert_type(CaseTypes, [T, ...]),
                    HeadTy = hd(NECaseTypes),
                    RestTys = tl(NECaseTypes),
                    lists:foreach(fun (TyTi) ->
                                    case types_equiv(Ctx, TyTi, HeadTy) of
                                        true ->
                                            ok;
                                        false ->
                                            type_error(Info, "fields do not have the same type")
                                    end
                                  end,
                                  RestTys),
                    HeadTy;
                _ ->
                    type_error(Info, "expected variant type")
            end;
        {tag, Info, L, Ti, TyT} ->
            case simplify_type(Ctx, TyT) of
                {variant, FieldTypes} ->
                    case lists:keyfind(L, 1, FieldTypes) of
                        {L, TyTiExpected} ->
                            TyTi = type_of(Ctx, Ti),
                            case types_equiv(Ctx, TyTi, TyTiExpected) of
                                true ->
                                    TyT;
                                false ->
                                    type_error(Info, "field ~p does not have the expected type ~p",
                                              [L, TyTiExpected])
                            end;
                        false ->
                            type_error(Info, "label ~p not found", L)
                    end;
                _ ->
                    type_error(Info, "annotation is not a variant type")
            end;
        {var, Info, Index, _} ->
            ?syntax:get_type_from_context(Info, Ctx, Index);
        {abs, _, X, TyX, T2} ->
            NewCtx = ?syntax:add_binding(Ctx, X, {var_bind, TyX}),
            TyT2 = type_of(NewCtx, T2),
            {arr, TyX, ?syntax:type_shift(-1, TyT2)};
        {app, Info, T1, T2} ->
            TyT1 = type_of(Ctx, T1),
            TyT2 = type_of(Ctx, T2),
            case simplify_type(Ctx, TyT1) of
                {arr, TyT11, TyT12} ->
                    case types_equiv(Ctx, TyT2, TyT11) of
                        true ->
                            TyT12;
                        false ->
                            type_error(Info, "parameter type mismatch")
                    end;
                _ ->
                    type_error(Info, "arrow type expected")
            end;
        {let_, _, X, T1, T2} ->
            TyT1 = type_of(Ctx, T1),
            NewCtx = ?syntax:add_binding(Ctx, X, {var_bind, TyT1}),
            ?syntax:type_shift(-1, type_of(NewCtx, T2));
        {fix, Info, T1} ->
            TyT1 = type_of(Ctx, T1),
            case simplify_type(Ctx, TyT1) of
                {arr, TyT11, TyT12} ->
                    case types_equiv(Ctx, TyT12, TyT11) of
                        true ->
                            TyT12;
                        false ->
                            type_error(Info, "result of body not compatible with domain")
                    end;
                _ ->
                    type_error(Info, "arrow type expected")
            end;
        {string, _, _} -> string;
        {unit, _} -> unit;
        {ascribe, Info, T1, TyT} ->
            case types_equiv(Ctx, type_of(Ctx, T1), TyT) of
                true ->
                    TyT;
                false ->
                    type_error(Info, "body of as-term does not have the expected type")
            end;
        {record, _, Fields} ->
            FieldTypes = lists:map(fun ({Li, Ti}) ->
                                           {Li, type_of(Ctx, Ti)}
                                   end, Fields),
            {record, FieldTypes};
        {proj, Info, T1, L} ->
            case simplify_type(Ctx, type_of(Ctx, T1)) of
                {record, FieldTypes} ->
                    case lists:keyfind(L, 1, FieldTypes) of
                        {L, Ty} ->
                            Ty;
                        false ->
                            type_error(Info, "label ~p not found", [L])
                    end;
                _ ->
                    type_error(Info, "expected record type")
            end;
        {float, _, _} -> float;
        {timesfloat, Info, T1, T2} ->
            case {types_equiv(Ctx, type_of(Ctx, T1), float),
                  types_equiv(Ctx, type_of(Ctx, T2), float)}
            of
                {true, true} ->
                    float;
                _ ->
                    type_error(Info, "argument of timesfloat is not a number")
            end;
        {zero, _} -> nat;
        {succ, Info, T1} ->
            case types_equiv(Ctx, type_of(Ctx, T1), nat) of
                true ->
                    nat;
                false ->
                    type_error(Info, "argument of succ is not a number")
            end;
        {pred, Info, T1} ->
            case types_equiv(Ctx, type_of(Ctx, T1), nat) of
                true ->
                    nat;
                false ->
                    type_error(Info, "argument of pred is not a number")
            end;
        {is_zero, Info, T1} ->
            case types_equiv(Ctx, type_of(Ctx, T1), nat) of
                true ->
                    bool;
                false ->
                    type_error(Info, "argument of is_zero is not a number")
            end
    end.

-spec type_error(info(), string()) -> none().
type_error(Info, Error) ->
    Format = case Info of
                 {_,_} -> "~p:~p: ~ts\n";
                 _ when is_integer(Info) -> "~p: ~ts\n"
             end,
    io:format(Format, [Info, Error]),
    erlang:error({type_error, Info, Error}).

-spec type_error(info(), string(), [any()]) -> none().
type_error(Info, Format, Args) ->
    type_error(Info, io_lib:format(Format, Args)).
