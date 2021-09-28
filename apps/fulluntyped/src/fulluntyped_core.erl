%% @doc This module implements TAPL chapters 5, 6, 7.
%% See https://www.cis.upenn.edu/~bcpierce/tapl/ for the book.
-module(fulluntyped_core).

-export([eval/2,
         eval_binding/2,
         trace/0]).

-type binding() :: fulluntyped_syntax:binding().
-type context() :: fulluntyped_syntax:context().
-type term_() :: fulluntyped_syntax:term_().

-define(syntax, fulluntyped_syntax).

-spec eval(context(), term_()) -> term_().
eval(Ctx, T) ->
    %io:format("~p ~p\n", [Ctx, T]),
    try
        eval(Ctx, eval1(Ctx, T))
    catch throw:no_rule_applies ->
        T
    end.

-spec eval_binding(context(), binding()) -> binding().
eval_binding(Ctx, B) ->
    %io:format("~p ~p\n", [Ctx, B]),
    case B of
        {abb_bind, T} ->
            T_ = eval(Ctx, T),
            {abb_bind, T_};
        name_bind ->
            name_bind
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
        {var, Info, N, _} ->
            case ?syntax:get_binding(Info, Ctx, N) of
                {abb_bind, T1} -> T1;
                name_bind -> throw(no_rule_applies)
            end;
        {app, Info, T1, T2} ->
            T1IsVal = is_value(Ctx, T1),
            T2IsVal = is_value(Ctx, T2),
            case {T1, T1IsVal, T2IsVal} of
                {{abs, _, _, T12}, _, true} ->
                    ?syntax:term_subst_top(T2, T12);
                {_, true, _} ->
                    T2_ = eval1(Ctx, T2),
                    {app, Info, T1, T2_};
                _ ->
                    T1_ = eval1(Ctx, T1),
                    {app, Info, T1_, T2}
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
        {let_, Info, X, T1, T2} ->
            case is_value(Ctx, T1) of
                true ->
                    ?syntax:term_subst_top(T1, T2);
                false ->
                    T1_ = eval1(Ctx, T1),
                    {let_, Info, X, T1_, T2}
            end;
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
        {float, _, _} -> true;
        {string, _, _} -> true;
        {abs, _, _, _} -> true;
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

-spec trace() -> ok.
trace() ->
    dbg:tracer(),
    dbg:p(all, call),
    dbg:tpl(?MODULE, x),
    ok.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

eval_test_() ->
    [
     ?_assertEqual({zero, 0},
                   eval({zero, 0})),
     ?_assertEqual({succ, 0, {zero, 0}},
                   eval({succ, 0, {zero, 0}})),
     ?_assertEqual({zero, 0},
                   eval({pred, 0, {succ, 0, {zero, 0}}})),
     ?_assertEqual({zero, 0},
                   eval({pred, 0, {pred, 0, {succ, 0, {zero, 0}}}})),
     ?_assertEqual({succ, 0, {zero, 0}},
                   eval({succ, 0, {pred, 0, {succ, 0, {zero, 0}}}})),
     ?_assertEqual({true, 0},
                   eval({is_zero, 0, {zero, 0}})),
     ?_assertEqual({false, 0},
                   eval({is_zero, 0, {succ, 0, {zero, 0}}})),
     ?_assertEqual({zero, 0},
                   eval({'if', 0,
                         {is_zero, 0, {succ, 0, {zero, 0}}},
                         {succ, 0, {zero, 0}},
                         {zero, 0}})),
     ?_assertEqual({succ, 0, {zero, 0}},
                   eval({'if', 0,
                         {is_zero, 0, {zero, 0}},
                         {succ, 0, {zero, 0}},
                         {zero, 0}})),
     ?_assertEqual({succ, 0, {zero, 0}},
                   eval({'if', 0,
                         {is_zero, 0, {pred, 0, {succ, 0, {zero, 0}}}},
                         {succ, 0, {zero, 0}},
                         {zero, 0}})),
     ?_assertEqual({succ, 0, {zero, 0}},
                   eval({pred, 0, {'if', 0,
                                   {is_zero, 0, {zero, 0}},
                                   {succ, 0, {succ, 0, {zero, 0}}},
                                   {succ, 0, {zero, 0}}}})),
     ?_assertEqual({true, 0},
                   eval({is_zero, 0, {'if', 0,
                                      {is_zero, 0, {succ, 0, {zero, 0}}},
                                      {succ, 0, {zero, 0}},
                                      {zero, 0}}})),
     ?_assertEqual({false, 0},
                   eval({is_zero, 0, {'if', 0,
                                      {is_zero, 0, {zero, 0}},
                                      {succ, 0, {zero, 0}},
                                      {zero, 0}}}))
    ].

-endif. %% TEST
