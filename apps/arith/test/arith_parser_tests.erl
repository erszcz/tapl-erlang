-module(arith_parser_tests).

-include_lib("eunit/include/eunit.hrl").

data() ->
    {ok, Data} = file:read_file("test/test.f"),
    unicode:characters_to_list(Data).

parser_test() ->
    Data = data(),
    {ok, Tokens, _} = arith_lexer:string(Data),
    ?assertEqual({ok,[{eval,info,{true,info}},
                      {eval,info,
                       {'if',info,
                        {false,info},
                        {true,info},
                        {false,info}}},
                      {eval,info,{zero,info}},
                      {eval,info,{succ,info,{pred,info,{zero,info}}}},
                      {eval,info,
                       {is_zero,info,
                        {pred,info,
                         {succ,info,{succ,info,{zero,info}}}}}}]},
                 arith_parser:parse(Tokens)).
