-module(arith_lexer_tests).

-include_lib("eunit/include/eunit.hrl").

data() ->
    {ok, Data} = file:read_file("test/test.f"),
    unicode:characters_to_list(Data).

zero_test_() ->
    [?_assertEqual({ok, [{int_value,1,"1"}, {semi,1,";"}], 1},
                   arith_lexer:string("1;")),
     ?_assertEqual({ok, [{int_value,1,"0"}, {semi,1,";"}], 1},
                   arith_lexer:string("0;")),
     ?_assertEqual({ok, [{int_value,1,"9"}, {semi,1,";"}], 1},
                   arith_lexer:string("9;"))].

lexer_test() ->
    Data = data(),
    ?assertEqual({ok,[{comment_start,1,"/*"},
                      {ucid,1,"Examples"},
                      {lcid,1,"for"},
                      {lcid,1,"testing"},
                      {comment_end,1,"*/"},
                      {true,3,"true"},
                      {semi,3,";"},
                      {'if',4,"if"},
                      {false,4,"false"},
                      {then,4,"then"},
                      {true,4,"true"},
                      {else,4,"else"},
                      {false,4,"false"},
                      {semi,4,";"},
                      {int_value,6,"0"},
                      {semi,6,";"},
                      {succ,7,"succ"},
                      {lparen,7,"("},
                      {pred,7,"pred"},
                      {int_value,7,"0"},
                      {rparen,7,")"},
                      {semi,7,";"},
                      {iszero,8,"iszero"},
                      {lparen,8,"("},
                      {pred,8,"pred"},
                      {lparen,8,"("},
                      {succ,8,"succ"},
                      {lparen,8,"("},
                      {succ,8,"succ"},
                      {int_value,8,"0"},
                      {rparen,8,")"},
                      {rparen,8,")"},
                      {rparen,8,")"},
                      {semi,8,";"}],
                  9},
                 arith_lexer:string(Data)).
