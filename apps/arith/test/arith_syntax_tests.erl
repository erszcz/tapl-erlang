-module(arith_syntax_tests).

-include_lib("eunit/include/eunit.hrl").

data() ->
    {ok, Data} = file:read_file("test/test.f"),
    unicode:characters_to_list(Data).

identity_test() ->
    %% GIVEN a list of terms in the concrete syntax
    Data = data(),
    %% WHEN the list is parsed and pretty printed
    {ok, Tokens, _} = arith_lexer:string(Data),
    %% Commands, because the terms are wrapped as `{eval, ..., Term}'
    {ok, OrigCommands} = arith_parser:parse(Tokens),
    Pretty = lists:flatmap(fun ({eval, _, T}) ->
                                   arith_syntax:format_term(T) ++ ";\n"
                           end, OrigCommands),
    %?debugVal(Pretty, 1000),
    %% THEN tokenising and parsing the pretty printed terms results
    %% in the same parse tree
    {ok, NewTokens, _} = arith_lexer:string(Pretty),
    {ok, NewCommands} = arith_parser:parse(NewTokens),
    ?assertEqual(OrigCommands, NewCommands).
