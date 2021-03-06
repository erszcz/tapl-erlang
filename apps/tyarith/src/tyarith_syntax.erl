-module(tyarith_syntax).

-export([format_term/1, format_term/2,
         format_type/1, format_type/2]).

-export_type([command/0,
              term_/0,
              type/0]).

-type type() :: 'Bool'
              | 'Nat'.

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

-type command() :: {eval, info(), term()}.

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

-spec format_type(type()) -> string().
format_type(Type) ->
    format_type(Type, #{}).

-spec format_type(type(), map()) -> string().
format_type(Type, Opts) ->
    Doc = prettypr_a_type(true, Type),
    prettypr:format(Doc,
                    maps:get(paper_width, Opts, 80),
                    maps:get(line_width, Opts, 65)).

prettypr_type(Outer, Type) ->
    prettypr_a_type(Outer, Type).

prettypr_a_type(_Outer, 'Bool') -> prettypr:text("Bool");
prettypr_a_type(_Outer, 'Nat') -> prettypr:text("Nat");
prettypr_a_type(Outer, Type) ->
    prettypr:beside(prettypr:text("("),
                    prettypr:beside(prettypr_type(Outer, Type),
                                    prettypr:text(")"))).
