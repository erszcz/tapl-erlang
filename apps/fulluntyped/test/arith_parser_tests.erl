-module(arith_parser_tests).

-include_lib("eunit/include/eunit.hrl").

data() ->
    {ok, Data} = file:read_file("test/test.f"),
    unicode:characters_to_list(Data).

parser_test() ->
    Data = data(),
    {ok, Tokens, _} = arith_lexer:string(Data),
    ?assertEqual({ok,[{eval,{6,3},{true,{6,3}}},
                      {eval,
                       {7,1},
                       {'if',
                        {7,1},
                        {false,{7,4}},
                        {true,{7,15}},
                        {false,{7,25}}}},
                      {eval,{11,1},{zero,{11,1}}},
                      {eval,
                       {11,4},
                       {succ,{11,4},{pred,{11,10},{zero,{11,15}}}}},
                      {eval,
                       {12,1},
                       {is_zero,
                        {12,1},
                        {pred,
                         {12,9},
                         {succ,
                          {12,15},
                          {succ,{12,21},{zero,{12,26}}}}}}}]},
                 arith_parser:parse(Tokens)).
