%% @doc This module implements TAPL chapters 5, 6, 7.
%% See https://www.cis.upenn.edu/~bcpierce/tapl/ for the book.
-module(fullfsub_core).

-export([eval/2,
         eval_binding/2,
         type_of/2,
         types_equiv/3]).

-define(syntax, fullfsub_syntax).

-type binding() :: ?syntax:binding().
-type context() :: ?syntax:context().
-type index()   :: ?syntax:index().
-type info()    :: ?syntax:info().
-type term_()   :: ?syntax:term_().
-type ty()      :: ?syntax:ty().

-import(?syntax, [binding/1,
                  term_/1,
                  ty/1]).

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
        {if_, _, {true, _}, T2, _} ->
            T2;
        {if_, _, {false, _}, _, T3} ->
            T3;
        {if_, Info, T1, T2, T3} ->
            T1_ = eval1(Ctx, T1),
            {if_, Info, T1_, T2, T3};
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
        {unpack, Info, TyX, X, T1, T2} ->
            case T1 of
                {pack, _, Ty11, T12, _} ->
                    case is_value(Ctx, T12) of
                        true ->
                            T2_ = ?syntax:term_subst_top(?syntax:term_shift(1, T12), T2),
                            ?syntax:type_term_subst_top(Ty11, T2_);
                        false ->
                            throw(no_rule_applies)
                    end;
                _ ->
                    T1_ = eval1(Ctx, T1),
                    term_({unpack, Info, TyX, X, T1_, T2})
            end;
        {pack, Info, Ty1, T2, Ty3} ->
            T2_ = eval1(Ctx, T2),
            term_({pack, Info, Ty1, T2_, Ty3});
        {var, Info, Index, _} ->
            case ?syntax:get_binding(Info, Ctx, Index) of
                {tm_abb_bind, T1, _} ->
                    T1;
                _ ->
                    throw(no_rule_applies)
            end;
        {ty_app, _Info, {ty_abs, _, _, _, T11}, Ty2} ->
            ?syntax:type_term_subst_top(Ty2, T11);
        {ty_app, Info, T1, Ty2} ->
            T1_ = eval1(Ctx, T1),
            term_({ty_app, Info, T1_, Ty2});
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
        {string, _, _} -> true;
        {unit, _} -> true;
        {true, _} -> true;
        {false, _} -> true;
        {float, _, _} -> true;
        {abs, _, _, _, _} -> true;
        {record, _, Fields} ->
            lists:all(fun ({_Label, T1}) -> is_value(Ctx, T1) end, Fields);
        {pack, _, _, V1, _} ->
            is_value(Ctx, V1);
        {ty_abs, _, _, _, _} ->
            true;
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

-spec promote(context(), ty()) -> ty().
promote(Ctx, Ty) ->
    case Ty of
        {var, Index, _} ->
            case ?syntax:get_binding(dummy_info(), Ctx, Index) of
                {ty_var_bind, TyT} -> TyT;
                _ -> throw(no_rule_applies)
            end;
        _ ->
            throw(no_rule_applies)
    end.

dummy_info() ->
    {0,0}.

-spec is_ty_abb(context(), ty() | index()) -> boolean().
is_ty_abb(Ctx, {var, Index, _}) ->
    is_ty_abb(Ctx, Index);
is_ty_abb(Ctx, Index) when is_integer(Index) ->
    case ?syntax:get_binding(_DummyInfo = {1,1}, Ctx, Index) of
        {ty_abb_bind, _} -> true;
        _ -> false
    end;
is_ty_abb(_, _) ->
    false.

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
    TySIsTyAbb = is_ty_abb(Ctx, TyS),
    TyTIsTyAbb = is_ty_abb(Ctx, TyT),
    case {TyS, TyT} of
        {string, string} -> true;
        {top, top} -> true;
        {unit, unit} -> true;
        {{id, IdS}, {id, IdT}} -> IdS == IdT;
        {float, float} -> true;
        {{var, IndexS, _}, _} when TySIsTyAbb ->
            types_equiv(Ctx, get_ty_abb(Ctx, IndexS), TyT);
        {_, {var, IndexT, _}} when TyTIsTyAbb ->
            types_equiv(Ctx, TyS, get_ty_abb(Ctx, IndexT));
        {{var, Index, _}, {var, Index, _}} ->
            true;
        {{arr, TyS1, TyS2}, {arr, TyT1, TyT2}} ->
            types_equiv(Ctx, TyS1, TyT1)
            andalso
            types_equiv(Ctx, TyS2, TyT2);
        {bool, bool} -> true;
        {nat, nat} -> true;
        {{some, TyX1, TyS1, TyS2}, {some, _, TyT1, TyT2}} ->
            Ctx1 = ?syntax:add_name(Ctx, TyX1),
            types_equiv(Ctx, TyS1, TyT1) andalso types_equiv(Ctx1, TyS2, TyT2);
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
        {{all, TyX1, TyS1, TyS2}, {all, _, TyT1, TyT2}} ->
            Ctx1 = ?syntax:add_name(Ctx, TyX1),
            types_equiv(Ctx, TyS1, TyT1) andalso types_equiv(Ctx1, TyS2, TyT2);
        _ ->
            false
    end.

-spec subtype(context(), ty(), ty()) -> boolean().
subtype(Ctx, TyS, TyT) ->
    types_equiv(Ctx, TyS, TyT)
    orelse begin
               TyS_ = simplify_type(Ctx, TyS),
               TyT_ = simplify_type(Ctx, TyT),
               subtype_(Ctx, TyS_, TyT_)
           end.

%% When calling recursively call subtype/3 (without the trailing _) to check type equivalence, too.
-spec subtype_(context(), ty(), ty()) -> boolean().
subtype_(Ctx, TyS, TyT) ->
    case {TyS, TyT} of
        {_, top} ->
            true;
        {{arr, TyS1, TyS2}, {arr, TyT1, TyT2}} ->
            subtype(Ctx, TyT1, TyS1) andalso subtype(Ctx, TyS2, TyT2);
        {{record, FieldsS}, {record, FieldsT}} ->
            lists:all(fun ({TLabelI, TTyI}) ->
                        case lists:keyfind(TLabelI, 1, FieldsS) of
                            {TLabelI, STyI} ->
                                subtype(Ctx, STyI, TTyI);
                            false ->
                                false
                        end
                      end, FieldsT);
        {{var, _, _}, _} ->
            subtype(Ctx, promote(Ctx, TyS), TyT);
        {{all, TyX1, TyS1, TyS2}, {all, _, TyT1, TyT2}} ->
            (subtype(Ctx, TyS1, TyT1) andalso subtype(Ctx, TyT1, TyS1))
            andalso begin
                        Ctx1 = ?syntax:add_binding(Ctx, TyX1, binding({ty_var_bind, TyT1})),
                        subtype(Ctx1, TyS2, TyT2)
                    end;
        {{some, TyX1, TyS1, TyS2}, {some, _, TyT1, TyT2}} ->
            (subtype(Ctx, TyS1, TyT1) andalso subtype(Ctx, TyT1, TyS1))
            andalso begin
                        Ctx1 = ?syntax:add_binding(Ctx, TyX1, binding({ty_var_bind, TyT1})),
                        subtype(Ctx1, TyS2, TyT2)
                    end;
        _ ->
            false
    end.

%% Calculate the supertype (least upper bound) of the argument types.
-spec join(context(), ty(), ty()) -> ty().
join(Ctx, TyS, TyT) ->
    case {subtype(Ctx, TyS, TyT), subtype(Ctx, TyT, TyS)} of
        {true, _} ->
            TyT;
        {_, true} ->
            TyS;
        _ ->
            TyS_ = simplify_type(Ctx, TyS),
            TyT_ = simplify_type(Ctx, TyT),
            join_(Ctx, TyS_, TyT_)
    end.

-spec join_(context(), ty(), ty()) -> ty().
join_(Ctx, TyS, TyT) ->
    case {TyS, TyT} of
        {{record, FieldsS}, {record, FieldsT}} ->
            LabelsS = proplists:get_keys(FieldsS),
            LabelsT = proplists:get_keys(FieldsT),
            CommonLabels = lists:filter(fun (Label) -> lists:member(Label, LabelsT) end, LabelsS),
            CommonFields = lists:map(fun (Label) ->
                                             {Label, TySI} = lists:keyfind(Label, 1, FieldsS),
                                             {Label, TyTI} = lists:keyfind(Label, 1, FieldsT),
                                             {Label, join(Ctx, TySI, TyTI)}
                                     end, CommonLabels),
            ty({record, CommonFields});
        {{all, TyX, TyS1, _TyS2}, {all, _, TyT1, TyT2}} ->
            case not (subtype(Ctx, TyS1, TyT1) andalso subtype(Ctx, TyT1, TyS1)) of
                true ->
                    ty(top);
                false ->
                    Ctx1 = ?syntax:add_binding(Ctx, TyX, binding({ty_var_bind, TyT1})),
                    ty({all, TyX, TyS1, join(Ctx1, TyT1, TyT2)})
            end;
        {{arr, TyS1, TyS2}, {arr, TyT1, TyT2}} ->
            try
                ty({arr, meet(Ctx, TyS1, TyT1), join(Ctx, TyS2, TyT2)})
            catch
                throw:cannot_meet ->
                    ty(top)
            end;
        _ ->
            ty(top)
    end.

%% Calculate the subtype (greatest lower bound) of the argument types.
-spec meet(context(), ty(), ty()) -> ty().
meet(Ctx, TyS, TyT) ->
    case {subtype(Ctx, TyS, TyT), subtype(Ctx, TyT, TyS)} of
        {true, _} ->
            TyS;
        {_, true} ->
            TyT;
        _ ->
            TyS_ = simplify_type(Ctx, TyS),
            TyT_ = simplify_type(Ctx, TyT),
            meet_(Ctx, TyS_, TyT_)
    end.

-spec meet_(context(), ty(), ty()) -> ty().
meet_(Ctx, TyS, TyT) ->
    case {TyS, TyT} of
        {{record, FieldsS}, {record, FieldsT}} ->
            LabelsS = proplists:get_keys(FieldsS),
            LabelsT = proplists:get_keys(FieldsT),
            AllLabels = lists:append(LabelsS,
                                     lists:filter(fun (L) -> not lists:member(L, LabelsS) end,
                                                  LabelsT)),
            AllFields = lists:map(fun (L) ->
                                          case lists:member(L, AllLabels) of
                                              true ->
                                                  {L, TySI} = lists:keyfind(L, 1, FieldsS),
                                                  {L, TyTI} = lists:keyfind(L, 1, FieldsT),
                                                  {L, meet(Ctx, TySI, TyTI)};
                                              false ->
                                                  case lists:member(L, LabelsS) of
                                                      true ->
                                                          {L, TySI} = lists:keyfind(L, 1, FieldsS),
                                                          {L, TySI};
                                                      false ->
                                                          {L, TyTI} = lists:keyfind(L, 1, FieldsT),
                                                          {L, TyTI}
                                                  end
                                          end
                                  end,
                                  AllLabels),
            ty({record, AllFields});
        {{all, TyX, TyS1, _TyS2}, {all, _, TyT1, TyT2}} ->
            case not (subtype(Ctx, TyS1, TyT1) andalso subtype(Ctx, TyT1, TyS1)) of
                true ->
                    throw(cannot_meet);
                false ->
                    Ctx1 = ?syntax:add_binding(Ctx, TyX, binding({ty_var_bind, TyT1})),
                    ty({all, TyX, TyS1, meet(Ctx1, TyT1, TyT2)})
            end;
        _ ->
            throw(cannot_meet)
    end.

-spec lookup_constraint(context(), ty()) -> ty().
lookup_constraint(Ctx, TyS) ->
    TyS_ = simplify_type(Ctx, TyS),
    try
        lookup_constraint(Ctx, promote(Ctx, TyS_))
    catch
        _:no_rule_applies ->
            TyS_
    end.

-spec type_of(context(), term_()) -> ty().
type_of(Ctx, T) ->
    case T of
        {inert, _, Ty} ->
            Ty;
        {var, Info, Index, _} ->
            ?syntax:get_type_from_context(Info, Ctx, Index);
        {abs, _, X, TyX, T2} ->
            NewCtx = ?syntax:add_binding(Ctx, X, {var_bind, TyX}),
            TyT2 = type_of(NewCtx, T2),
            {arr, TyX, ?syntax:type_shift(-1, TyT2)};
        {app, Info, T1, T2} ->
            TyT1 = type_of(Ctx, T1),
            TyT2 = type_of(Ctx, T2),
            case lookup_constraint(Ctx, TyT1) of
                {arr, TyT11, TyT12} ->
                    case subtype(Ctx, TyT2, TyT11) of
                        true ->
                            TyT12;
                        false ->
                            type_error(Info, "parameter type mismatch")
                    end;
                _ ->
                    type_error(Info, "arrow type expected")
            end;
        {true, _} -> bool;
        {false, _} -> bool;
        {if_, Info, T1, T2, T3} ->
            case subtype(Ctx, type_of(Ctx, T1), bool) of
                true ->
                    join(Ctx, type_of(Ctx, T2), type_of(Ctx, T3));
                false ->
                    type_error(Info, "guard of conditional not a boolean")
            end;
        {record, _, Fields} ->
            FieldTypes = lists:map(fun ({Li, Ti}) ->
                                           {Li, type_of(Ctx, Ti)}
                                   end, Fields),
            {record, FieldTypes};
        {proj, Info, T1, L} ->
            case lookup_constraint(Ctx, type_of(Ctx, T1)) of
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
        {let_, _, X, T1, T2} ->
            TyT1 = type_of(Ctx, T1),
            NewCtx = ?syntax:add_binding(Ctx, X, {var_bind, TyT1}),
            ?syntax:type_shift(-1, type_of(NewCtx, T2));
        {fix, Info, T1} ->
            TyT1 = type_of(Ctx, T1),
            case lookup_constraint(Ctx, TyT1) of
                {arr, TyT11, TyT12} ->
                    case subtype(Ctx, TyT12, TyT11) of
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
            case subtype(Ctx, type_of(Ctx, T1), TyT) of
                true ->
                    TyT;
                false ->
                    type_error(Info, "body of as-term does not have the expected type")
            end;
        {float, _, _} -> float;
        {timesfloat, Info, T1, T2} ->
            case {subtype(Ctx, type_of(Ctx, T1), float),
                  subtype(Ctx, type_of(Ctx, T2), float)}
            of
                {true, true} ->
                    float;
                _ ->
                    type_error(Info, "argument of timesfloat is not a number")
            end;
        {ty_abs, _Info, TyX, TyT1, T2} ->
            Ctx1 = ?syntax:add_binding(Ctx, TyX, binding({ty_var_bind, TyT1})),
            TyT2 = type_of(Ctx1, T2),
            ty({all, TyX, TyT1, TyT2});
        {ty_app, Info, T1, TyT2} ->
            TyT1 = type_of(Ctx, T1),
            case lookup_constraint(Ctx, TyT1) of
                {all, _, TyT11, TyT12} ->
                    case subtype(Ctx, TyT2, TyT11) of
                        false ->
                            type_error(Info, "type parameter type mismatch");
                        true ->
                            ?syntax:type_subst_top(TyT2, TyT12)
                    end;
                _ ->
                    type_error(Info, "universal type expected")
            end;
        {zero, _} -> nat;
        {succ, Info, T1} ->
            case subtype(Ctx, type_of(Ctx, T1), nat) of
                true ->
                    nat;
                false ->
                    type_error(Info, "argument of succ is not a number")
            end;
        {pred, Info, T1} ->
            case subtype(Ctx, type_of(Ctx, T1), nat) of
                true ->
                    nat;
                false ->
                    type_error(Info, "argument of pred is not a number")
            end;
        {is_zero, Info, T1} ->
            case subtype(Ctx, type_of(Ctx, T1), nat) of
                true ->
                    bool;
                false ->
                    type_error(Info, "argument of is_zero is not a number")
            end;
        {pack, Info, TyT1, T2, TyT} ->
            case simplify_type(Ctx, TyT) of
                {some, _TyY, TyBound, TyT2} ->
                    case subtype(Ctx, TyT1, TyBound) of
                        false ->
                            type_error(Info, "hidden type not a subtype of bound");
                        true ->
                            TyU = type_of(Ctx, T2),
                            TyU_ = ?syntax:type_subst_top(TyT1, TyT2),
                            case subtype(Ctx, TyU, TyU_) of
                                true ->
                                    TyT;
                                false ->
                                    type_error(Info, "doesn't match declared type")
                            end
                    end;
                _ ->
                    type_error(Info, "existential type expected")
            end;
        {unpack, Info, TyX, X, T1, T2} ->
            TyT1 = type_of(Ctx, T1),
            case lookup_constraint(Ctx, TyT1) of
                {some, _TyY, TyBound, TyT11} ->
                    Ctx1 = ?syntax:add_binding(Ctx, TyX, binding({ty_var_bind, TyBound})),
                    Ctx2 = ?syntax:add_binding(Ctx1, X, binding({var_bind, TyT11})),
                    TyT2 = type_of(Ctx2, T2),
                    ?syntax:type_shift(-2, TyT2);
                _ ->
                    type_error(Info, "existential type expected")
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

-spec eval_binding(context(), binding()) -> binding().
eval_binding(Ctx, B) ->
    case B of
        {tm_abb_bind, T, Ty} ->
            T_ = eval(Ctx, T),
            {tm_abb_bind, T_, Ty};
        Bind ->
            Bind
    end.
