-module(tyarith_core).

-export([eval/1,
         type_of/1,
         trace/0]).

-type term_() :: tyarith_syntax:term_().
-type type() :: tyarith_syntax:type().

-spec eval(term_()) -> term_().
eval(T) ->
    try
        eval(eval1(T))
    catch throw:no_rule_applies ->
        T
    end.

-spec eval1(term_()) -> term_().
eval1(T) ->
    case T of
        {'if', _, {true, _}, T2, _} ->
            T2;
        {'if', _, {false, _}, _, T3} ->
            T3;
        {'if', Info, T1, T2, T3} ->
            T1_ = eval1(T1),
            {'if', Info, T1_, T2, T3};
        {succ, Info, T1} ->
            T1_ = eval1(T1),
            {succ, Info, T1_};
        {pred, _, {zero, _}} ->
            {zero, 0};
        {pred, _, {succ, _, T1}} ->
            T1;
        {pred, Info, T1} ->
            T1_ = eval1(T1),
            {pred, Info, T1_};
        {is_zero, _, {zero, _}} ->
            {true, 0};
        {is_zero, _, {succ, _, _}} ->
            {false, 0};
        {is_zero, Info, T1} ->
            T1_ = eval1(T1),
            {is_zero, Info, T1_};
        _ ->
            erlang:throw(no_rule_applies)
    end.

-spec type_of(term_()) -> type().
type_of(T) ->
    case T of
        {true, _} -> 'Bool';
        {false, _} -> 'Bool';
        {'if', _, T1, T2, T3} ->
            case type_of(T1) of
                'Bool' ->
                    case {type_of(T2), type_of(T3)} of
                        {Ty, Ty} ->
                            Ty;
                        _ ->
                            erlang:error(cond_arms_of_different_types, [T])
                    end;
                _ ->
                    erlang:error(cond_guard_not_a_boolean, [T])
            end;
        {zero, _} -> 'Nat';
        {succ, _, T1} ->
            case type_of(T1) of
                'Nat' -> 'Nat';
                _ -> erlang:error(succ_arg_not_a_number, [T])
            end;
        {pred, _, T1} ->
            case type_of(T1) of
                'Nat' -> 'Nat';
                _ -> erlang:error(pred_arg_not_a_number, [T])
            end;
        {is_zero, _, T1} ->
            case type_of(T1) of
                'Nat' -> 'Bool';
                _ -> erlang:error(is_zero_arg_not_a_number, [T])
            end
    end.

trace() ->
    dbg:tracer(),
    dbg:p(all, call),
    dbg:tpl(?MODULE, x).

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
