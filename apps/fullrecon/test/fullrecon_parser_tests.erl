-module(fullrecon_parser_tests).

-include_lib("eunit/include/eunit.hrl").

data() ->
    {ok, Data} = file:read_file("test/test.f"),
    unicode:characters_to_list(Data).

parser_test() ->
    Data = data(),
    {ok, Tokens, _} = fullrecon_lexer:string(Data),
    {ok, ParserF} = fullrecon_parser:parse(Tokens),
    ?assertEqual({[{eval,{3,2},{let_,{3,2},"x",{true,{3,8}},{var,{3,16},0,1}}},
                   {eval,{5,1},{abs,{5,1},"x",{ok,bool},{var,{5,16},0,1}}},
                   {eval,{6,2},
                    {app,{6,2},
                     {abs,{6,2},
                      "x",
                      {ok,{arr,bool,bool}},
                      {if_,{6,23},
                       {app,{6,26},{var,{6,26},0,1},{false,{6,28}}},
                       {true,{6,39}},
                       {false,{6,49}}}},
                     {abs,{7,4},
                      "x",
                      {ok,bool},
                      {if_,{7,19},
                       {var,{7,22},0,1},
                       {false,{7,29}},
                       {true,{7,40}}}}}},
                   {eval,{9,1},{abs,{9,1},"x",{ok,nat},{succ,{9,15},{var,{9,20},0,1}}}},
                   {eval,{10,2},
                    {app,{10,2},
                     {abs,{10,2},
                      "x",
                      {ok,nat},
                      {succ,{10,16},{succ,{10,22},{var,{10,27},0,1}}}},
                     {succ,{10,32},{zero,{10,37}}}}},
                   {eval,{12,1},{abs,{12,1},"x",{ok,{id,"A"}},{var,{12,13},0,1}}},
                   {eval,{15,2},
                    {abs,{15,2},
                     "x",
                     {ok,{id,"X"}},
                     {abs,{15,14},
                      "y",
                      {ok,{arr,{id,"X"},{id,"X"}}},
                      {app,{15,29},{var,{15,29},0,2},{var,{15,31},1,2}}}}},
                   {eval,{16,2},
                    {app,{16,2},
                     {abs,{16,2},
                      "x",
                      {ok,{arr,{id,"X"},{id,"X"}}},
                      {app,{16,17},{var,{16,17},0,1},{zero,{16,19}}}},
                     {abs,{16,23},"y",{ok,nat},{var,{16,37},0,1}}}},
                   {eval,{20,2},
                    {abs,{20,2},"x",none,{app,{20,12},{var,{20,12},0,1},{zero,{20,14}}}}},
                   {eval,{21,1},
                    {let_,{21,1},
                     "f",
                     {abs,{21,9},"x",none,{var,{21,19},0,1}},
                     {app,{21,25},
                      {app,{21,25},{var,{21,25},0,1},{var,{21,27},0,1}},
                      {app,{21,31},{var,{21,31},0,1},{zero,{21,33}}}}}},
                   {eval,{22,1},
                    {let_,{22,1},
                     "g",
                     {abs,{22,9},"x",none,{succ,{22,19},{zero,{22,19}}}},
                     {app,{22,24},
                      {var,{22,24},0,1},
                      {app,{22,27},{var,{22,27},0,1},{var,{22,29},0,1}}}}}],
                  []},
                 ParserF([])).
