-module(arith_syntax).

-export(['if'/4,
         succ/2,
         pred/2,
         is_zero/2,
         eval/1]).

-export([format_term/1, format_term/2]).

-export_type([command/0,
              term_/0]).

-type info() :: integer().

-type term_() :: {true, info()}
               | {false, info()}
               | {'if', info(), term_(), term_(), term_()}
               | {zero, info()}
               | {succ, info(), term()}
               | {pred, info(), term()}
               | {is_zero, info(), term()}.
%% TAPL `term' type, but `term()' is a builtin type in Erlang,
%% hence the name `term_()'.

'if'({'if', Info}, Cond, Then, Else) ->
    {'if', Info, Cond, Then, Else}.

succ({succ, Info}, T) ->
    {succ, Info, T}.

pred({pred, Info}, T) ->
    {pred, Info, T}.

is_zero({iszero, Info}, T) ->
    {is_zero, Info, T}.

-type command() :: {eval, info(), term()}.

eval(T) -> {eval, term_info(T), T}.

term_info(T) ->
    case T of
        {true, Info} -> Info;
        {false, Info} -> Info;
        {'if', Info, _, _, _} -> Info;
        {zero, Info} -> Info;
        {succ, Info, _} -> Info;
        {pred, Info, _} -> Info;
        {is_zero, Info, _} -> Info
    end.

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
