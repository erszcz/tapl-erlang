%% @doc This module implements TAPL chapters 5, 6, 7.
%% See https://www.cis.upenn.edu/~bcpierce/tapl/ for the book.
-module(recon_core).

-export([eval/2,
         type_of/2,
         types_equiv/3,
         type_error/2, type_error/3]).

-define(syntax, recon_syntax).

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
        {abs, _, _, _, _} -> true;
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
                            type_error(Info,
                                       "result of body~n~n    ~p~n~n"
                                       "not compatible with domain~n~n    ~p~n",
                                       [TyT12, TyT11])
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

-spec type_error(info(), _) -> none().
type_error(Info, Error) ->
    {Line, Col} = Info,
    io:format("~p:~p: ~ts~n", [Line, Col, Error]),
    erlang:error({type_error, Info, Error}).

-spec type_error(info(), string(), [any()]) -> none().
type_error(Info, Format, Args) ->
    type_error(Info, io_lib:format(Format, Args)).
