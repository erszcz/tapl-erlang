-module(fulluntyped_lexer_tests).

-include_lib("eunit/include/eunit.hrl").

data() ->
    {ok, Data} = file:read_file("test/test.f"),
    unicode:characters_to_list(Data).

zero_test_() ->
    [?_assertEqual({ok, [{int_value,{1,1},"1"}, {semi,{1,2}}], 1},
                   begin
                       fulluntyped_lexer:reset(),
                       fulluntyped_lexer:string("1;")
                   end),
     ?_assertEqual({ok, [{int_value,{1,1},"0"}, {semi,{1,2}}], 1},
                   begin
                       fulluntyped_lexer:reset(),
                       fulluntyped_lexer:string("0;")
                   end),
     ?_assertEqual({ok, [{int_value,{1,1},"9"}, {semi,{1,2}}], 1},
                   begin
                       fulluntyped_lexer:reset(),
                       fulluntyped_lexer:string("9;")
                   end)].

lexer_test() ->
    Data = data(),
    fulluntyped_lexer:reset(),
    ?assertEqual({ok,[{comment,{1,3},"/* 1 or 2 examples for testing */"},
                      {comment,{2,1},"/* multi\n   line\n   comment */"},
                      {true,{6,3}},
                      {semi,{6,9}},
                      {'if',{7,1}},
                      {false,{7,4}},
                      {then,{7,10}},
                      {true,{7,15}},
                      {else,{7,20}},
                      {false,{7,25}},
                      {semi,{7,30}},
                      {comment,{9,1},"/* another comment */"},
                      {int_value,{11,1},"0"},
                      {semi,{11,2}},
                      {succ,{11,4}},
                      {lparen,{11,9}},
                      {pred,{11,10}},
                      {int_value,{11,15},"0"},
                      {rparen,{11,16}},
                      {semi,{11,17}},
                      {iszero,{12,1}},
                      {lparen,{12,8}},
                      {pred,{12,9}},
                      {lparen,{12,14}},
                      {succ,{12,15}},
                      {lparen,{12,20}},
                      {succ,{12,21}},
                      {int_value,{12,26},"0"},
                      {rparen,{12,27}},
                      {rparen,{12,28}},
                      {rparen,{12,29}},
                      {semi,{12,30}}],
                  13},
                 fulluntyped_lexer:string(Data)).
