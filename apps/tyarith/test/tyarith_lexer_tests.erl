-module(tyarith_lexer_tests).

-include_lib("eunit/include/eunit.hrl").

data() ->
    {ok, Data} = file:read_file("test/test.f"),
    unicode:characters_to_list(Data).

zero_test_() ->
    [?_assertEqual({ok, [{int_value,1,"1"}, {semi,1,";"}], 1},
                   tyarith_lexer:string("1;")),
     ?_assertEqual({ok, [{int_value,1,"0"}, {semi,1,";"}], 1},
                   tyarith_lexer:string("0;")),
     ?_assertEqual({ok, [{int_value,1,"9"}, {semi,1,";"}], 1},
                   tyarith_lexer:string("9;"))].

lexer_test() ->
    Data = data(),
    ?assertEqual({ok,[{comment,1,"/* 1 or 2 examples for testing */"},
                      {true,3,"true"},
                      {semi,3,";"},
                      {'if',4,"if"},
                      {false,4,"false"},
                      {then,4,"then"},
                      {true,4,"true"},
                      {else,4,"else"},
                      {false,4,"false"},
                      {semi,4,";"},
                      {comment,6,"/* another comment */"},
                      {int_value,8,"0"},
                      {semi,8,";"},
                      {succ,9,"succ"},
                      {lparen,9,"("},
                      {pred,9,"pred"},
                      {int_value,9,"0"},
                      {rparen,9,")"},
                      {semi,9,";"},
                      {iszero,10,"iszero"},
                      {lparen,10,"("},
                      {pred,10,"pred"},
                      {lparen,10,"("},
                      {succ,10,"succ"},
                      {lparen,10,"("},
                      {succ,10,"succ"},
                      {int_value,10,"0"},
                      {rparen,10,")"},
                      {rparen,10,")"},
                      {rparen,10,")"},
                      {semi,10,";"}],
                  11},
                 tyarith_lexer:string(Data)).
