-module(fulluntyped_syntax_tests).

-include_lib("eunit/include/eunit.hrl").

data() ->
    {ok, Data} = file:read_file("test/test.f"),
    unicode:characters_to_list(Data).

identity_test() ->
    %% GIVEN a list of terms in the concrete syntax
    Data = data(),
    %% WHEN the list is parsed and pretty printed
    {ok, Tokens, _} = fulluntyped_lexer:string(Data),
    %% Commands, because the terms are wrapped as `{eval, ..., Term}'
    {ok, OrigCommands} = fulluntyped_parser:parse(Tokens),
    Pretty = lists:flatmap(fun ({eval, _, T}) ->
                                   fulluntyped_syntax:format_term(T) ++ ";\n"
                           end, OrigCommands),
    %?debugVal(Pretty, 1000),
    %% THEN tokenising and parsing the pretty printed terms results
    %% in the same parse tree modulo the token locations
    {ok, NewTokens, _} = fulluntyped_lexer:string(Pretty),
    {ok, NewCommands} = fulluntyped_parser:parse(NewTokens),
    OrigCommandsNoInfo = strip_info(OrigCommands),
    NewCommandsNoInfo = strip_info(NewCommands),
    ?assertEqual(OrigCommandsNoInfo, NewCommandsNoInfo).

strip_info([]) -> [];
strip_info([{eval, _Info, T} | Rest]) -> [{eval, info, strip_info(T)} | strip_info(Rest)];
strip_info(T) ->
    case T of
        {true, _Info} -> {true, info};
        {false, _Info} -> {true, info};
        {if_, _Info, T1, T2, T3} -> {if_, info, strip_info(T1), strip_info(T2), strip_info(T3)};
        {zero, _Info} -> {zero, info};
        {succ, _Info, T1} -> {succ, info, strip_info(T1)};
        {pred, _Info, T1} -> {pred, info, strip_info(T1)};
        {is_zero, _Info, T1} -> {is_zero, info, strip_info(T1)}
    end.
