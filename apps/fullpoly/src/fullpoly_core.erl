%% @doc This module implements TAPL chapters 5, 6, 7.
%% See https://www.cis.upenn.edu/~bcpierce/tapl/ for the book.
-module(fullpoly_core).

-export([eval/2,
         recon/3,
         unify/4,
         apply_subst/2,
         uvar_gen/0,
         empty_constraints/0,
         combine_constraints/2]).

-export_type([uvar_gen/0]).

-define(syntax, fullpoly_syntax).

-type context() :: ?syntax:context().
-type info()    :: ?syntax:info().
-type term_()   :: ?syntax:term_().
-type ty()      :: ?syntax:ty().

-import(?syntax, [binding/1,
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
        {let_, Info, X, T1, T2} ->
            case is_value(Ctx, T1) of
                true ->
                    ?syntax:term_subst_top(T1, T2);
                false ->
                    T1_ = eval1(Ctx, T1),
                    {let_, Info, X, T1_, T2}
            end;
        {if_, _, {true, _}, T2, _} ->
            T2;
        {if_, _, {false, _}, _, T3} ->
            T3;
        {if_, Info, T1, T2, T3} ->
            T1_ = eval1(Ctx, T1),
            {if_, Info, T1_, T2, T3};
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
        _ ->
            throw(no_rule_applies)
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

-type constraints() :: [{ty(), ty()}].

-spec empty_constraints() -> constraints().
empty_constraints() -> [].

-spec combine_constraints(constraints(), constraints()) -> constraints().
combine_constraints(Cs1, Cs2) -> lists:append(Cs1, Cs2).

-type uvar_gen() :: fun(() -> next_uvar()).
-type next_uvar() :: {string(), uvar_gen()}.

-spec uvar_gen() -> next_uvar().
uvar_gen() ->
    F = fun F(N) ->
                {"?X" ++ integer_to_list(N), fun () -> F(N+1) end}
        end,
    element(2, F(-1)).

-spec recon(Ctx, NextUVar, T) -> R when
      Ctx :: context(),
      NextUVar :: uvar_gen(),
      T :: term_(),
      R :: {ty(), uvar_gen(), constraints()}.
recon(Ctx, NextUVar, T) ->
    case T of
        {var, Info, Index, _} ->
            Ty = ?syntax:get_type_from_context(Info, Ctx, Index),
            {Ty, NextUVar, []};
        {abs, _Info, X, {ok, Ty1}, T2} ->
            NewCtx = ?syntax:add_binding(Ctx, X, binding({var_bind, Ty1})),
            {Ty2, NextUVar2, Cs2} = recon(NewCtx, NextUVar, T2),
            {ty({arr, Ty1, Ty2}), NextUVar2, Cs2};
        {abs, _Info, X, none, T2} ->
            {U, NextUVar0} = NextUVar(),
            TyX = ty({id, U}),
            NewCtx = ?syntax:add_binding(Ctx, X, binding({var_bind, TyX})),
            {Ty2, NextUVar2, Cs2} = recon(NewCtx, NextUVar0, T2),
            {ty({arr, TyX, Ty2}), NextUVar2, Cs2};
        {app, _Info, T1, T2} ->
            {Ty1, NextUVar1, Cs1} = recon(Ctx, NextUVar, T1),
            {Ty2, NextUVar2, Cs2} = recon(Ctx, NextUVar1, T2),
            {TyX, NextUVar3} = NextUVar2(),
            NewCs = [{Ty1, ty({arr, Ty2, ty({id, TyX})})}],
            {ty({id, TyX}), NextUVar3, lists:append([NewCs, Cs1, Cs2])};
        {let_, _Info, X, T1, T2} ->
            case is_value(Ctx, T1) of
                false ->
                    {Ty1, NextUVar1, Cs1} = recon(Ctx, NextUVar, T1),
                    NewCtx = ?syntax:add_binding(Ctx, X, binding({var_bind, Ty1})),
                    {Ty2, NextUVar2, Cs2} = recon(NewCtx, NextUVar1, T2),
                    {Ty2, NextUVar2, Cs1 ++ Cs2};
                true ->
                    recon(Ctx, NextUVar, ?syntax:term_subst_top(T1, T2))
            end;
        {zero, _Info} ->
            {ty(nat), NextUVar, []};
        {succ, _Info, T1} ->
            {Ty1, NextUVar1, Cs1} = recon(Ctx, NextUVar, T1),
            {ty(nat), NextUVar1, [{Ty1, ty(nat)} | Cs1]};
        {pred, _Info, T1} ->
            {Ty1, NextUVar1, Cs1} = recon(Ctx, NextUVar, T1),
            {ty(nat), NextUVar1, [{Ty1, ty(nat)} | Cs1]};
        {is_zero, _Info, T1} ->
            {Ty1, NextUVar1, Cs1} = recon(Ctx, NextUVar, T1),
            {ty(bool), NextUVar1, [{Ty1, ty(nat)} | Cs1]};
        {true, _Info} ->
            {ty(bool), NextUVar, []};
        {false, _Info} ->
            {ty(bool), NextUVar, []};
        {if_, _Info, T1, T2, T3} ->
            {Ty1, NextUVar1, Cs1} = recon(Ctx, NextUVar, T1),
            {Ty2, NextUVar2, Cs2} = recon(Ctx, NextUVar1, T2),
            {Ty3, NextUVar3, Cs3} = recon(Ctx, NextUVar2, T3),
            NewCs = [{Ty1, ty(bool)}, {Ty2, Ty3}],
            {Ty3, NextUVar3, lists:append([NewCs, Cs1, Cs2, Cs3])}
    end.

-spec subst_in_type(string(), ty(), ty()) -> ty().
subst_in_type(TyX, TyT, TyS) ->
    F = fun
            F({arr, TyS1, TyS2}) ->
                ty({arr, F(TyS1), F(TyS2)});
            F(nat) ->
                ty(nat);
            F(bool) ->
                ty(bool);
            F({id, S}) ->
                case S of
                    TyX -> TyT;
                    _ -> ty({id, S})
                end
        end,
    F(TyS).

-spec apply_subst(constraints(), ty()) -> ty().
apply_subst(Cs, TyT) ->
    lists:foldl(fun
                    ({{id, TyX}, TyC2}, TyS) ->
                        subst_in_type(TyX, TyC2, TyS);
                    (_, TyS) ->
                        TyS
                end, TyT, lists:reverse(Cs)).

-spec subst_in_constraints(string(), ty(), constraints()) -> constraints().
subst_in_constraints(TyX, TyT, Cs) ->
    lists:map(fun ({TyS1, TyS2}) ->
                      {subst_in_type(TyX, TyT, TyS1), subst_in_type(TyX, TyT, TyS2)}
              end, Cs).

-spec occurs_in(string(), ty()) -> boolean().
occurs_in(TyX, TyT) ->
    case TyT of
        {arr, Ty1, Ty2} ->
            occurs_in(TyX, Ty1) orelse occurs_in(TyX, Ty2);
        nat ->
            false;
        bool ->
            false;
        {id, S} ->
            S == TyX
    end.

-spec unify(info(), context(), string(), constraints()) -> constraints().
unify(Info, Ctx, Msg, Cs) ->
    case Cs of
        [] ->
            [];
        %% This magic is to avoid list exhaustiveness checking limitations in Gradualizer.
        %% I.e. it can easily check exhaustiveness on disjoint unions,
        %% but not so easily on complex list patterns.
        [C | Rest] ->
            case C of
                {TyS, {id, TyX}} ->
                    case {TyS == ty({id, TyX}), occurs_in(TyX, TyS)} of
                        {true, _} ->
                            unify(Info, Ctx, Msg, Rest);
                        {false, true} ->
                            erlang:error({circular_constraints, Info, Msg});
                        _ ->
                            lists:append(unify(Info, Ctx, Msg, subst_in_constraints(TyX, TyS, Rest)),
                                         [{ty({id, TyX}), TyS}])
                    end;
                {{id, TyX}, TyT} ->
                    case {TyT == ty({id, TyX}), occurs_in(TyX, TyT)} of
                        {true, _} ->
                            unify(Info, Ctx, Msg, Rest);
                        {false, true} ->
                            erlang:error({circular_constraints, Info, Msg});
                        _ ->
                            lists:append(unify(Info, Ctx, Msg, subst_in_constraints(TyX, TyT, Rest)),
                                         [{ty({id, TyX}), TyT}])
                    end;
                {nat, nat} ->
                    unify(Info, Ctx, Msg, Rest);
                {bool, bool} ->
                    unify(Info, Ctx, Msg, Rest);
                {{arr, TyS1, TyS2}, {arr, TyT1, TyT2}} ->
                    unify(Info, Ctx, Msg, [ {TyS1, TyT1}, {TyS2, TyT2} | Rest]);
                _ ->
                    erlang:error({unsolvable_constraints, Info, Cs})
            end
    end.
