%% @doc This module implements TAPL 4 - the implementation of arithmetic expressions.
%% See https://www.cis.upenn.edu/~bcpierce/tapl/ for the book.
-module(arith).
-compile([export_all]).

-type info() :: erl_anno:location().

%% This type is called `term' in TAPL, but `term()' is a builtin type in Erlang.
-type term_() :: {true, info()}
               | {false, info()}
               | {'if', info(), term_(), term_(), term_()}
               | {zero, info()}
               | {succ, info(), term()}
               | {pred, info(), term()}
               | {is_zero, info(), term()}.

-spec is_numeric_val(term_()) -> boolean().
is_numeric_val({zero, _}) -> true;
is_numeric_val({succ, _, T1}) -> is_numeric_val(T1);
is_numeric_val(_) -> false.

-spec is_val(term_()) -> boolean().
is_val({true, _, _})  -> true;
is_val({false, _, _}) -> true;
is_val(T) -> is_numeric_val(T).

-spec eval(term_()) -> term_().
eval(T) ->
    try
        eval(eval1(T))
    catch error:no_rule_applies ->
        T
    end.

-spec eval1(term_()) -> term_() | no_rule_applies.
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
        {pred, _, T1} ->
            case T1 of
                {succ, _, NV1} ->
                    case is_numeric_val(NV1) of
                        true ->
                            NV1;
                        false ->
                            eval1_pred_non_numeric(T)
                    end;
                _ ->
                    eval1_pred_non_numeric(T)
            end;
        {is_zero, _, {zero, _}} ->
            {true, 0};
        {is_zero, _, T1} ->
            case T1 of
                {succ, _, NV1} ->
                    case is_numeric_val(NV1) of
                        true ->
                            {false, 0};
                        _ ->
                            eval1_is_zero_non_numeric(T)
                    end;
                _ ->
                    eval1_is_zero_non_numeric(T)
            end;
        _ ->
            erlang:error(no_rule_applies)
    end.

eval1_pred_non_numeric({pred, Info, T1}) ->
    T1_ = eval1(T1),
    {pred, Info, T1_}.

eval1_is_zero_non_numeric({is_zero, Info, T1}) ->
    T1_ = eval1(T1),
    {is_zero, Info, T1_}.

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
