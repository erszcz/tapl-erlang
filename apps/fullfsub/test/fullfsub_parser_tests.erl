-module(fullfsub_parser_tests).

-include_lib("eunit/include/eunit.hrl").

data() ->
    {ok, Data} = file:read_file("test/test.f"),
    unicode:characters_to_list(Data).

parser_test() ->
    Data = data(),
    {ok, Tokens, _} = fullfsub_lexer:string(Data),
    {ok, ParserF} = fullfsub_parser:parse(Tokens),
    ?assertEqual({[{eval,{3,2},{abs,{3,2},"x",top,{var,{3,16},0,1}}},
                   {eval,{4,4},
                    {app,{4,4},
                     {abs,{4,4},"x",top,{var,{4,18},0,1}},
                     {abs,{4,22},"x",top,{var,{4,36},0,1}}}},
                   {eval,{5,3},
                    {app,{5,3},
                     {abs,{5,3},"x",{arr,top,top},{var,{5,22},0,1}},
                     {abs,{5,26},"x",top,{var,{5,40},0,1}}}},
                   {eval,{8,2},
                    {app,{8,2},
                     {abs,{8,2},
                      "r",
                      {record,[{"x",{arr,top,top}}]},
                      {app,{8,26},
                       {proj,{8,26},{var,{8,25},0,1},"x"},
                       {proj,{8,30},{var,{8,29},0,1},"x"}}},
                     {record,{9,3},
                      [{"x",{abs,{9,6},"z",top,{var,{9,19},0,1}}},
                       {"y",{abs,{9,24},"z",top,{var,{9,37},0,1}}}]}}},
                   {eval,{12,1},{string,{12,1},"hello"}},
                   {eval,{14,1},{unit,{14,1}}},
                   {eval,{16,1},{abs,{16,1},"x",{id,"A"},{var,{16,13},0,1}}},
                   {eval,{18,1},{let_,{18,1},"x",{true,{18,7}},{var,{18,15},0,1}}},
                   {eval,{20,1},{record,{20,1},[{"x",{true,{20,4}}},{"y",{false,{20,12}}}]}},
                   {eval,{21,18},
                    {proj,{21,18},
                     {record,{21,1},[{"x",{true,{21,4}}},{"y",{false,{21,12}}}]},
                     "x"}},
                   {eval,{22,1},{record,{22,1},[{"1",{true,{22,2}}},{"2",{false,{22,8}}}]}},
                   {eval,{23,14},
                    {proj,{23,14},
                     {record,{23,1},[{"1",{true,{23,2}}},{"2",{false,{23,8}}}]},
                     "1"}},
                   {eval,{26,1},
                    {if_,{26,1},
                     {true,{26,4}},
                     {record,{26,14},
                      [{"x",{true,{26,17}}},
                       {"y",{false,{26,24}}},
                       {"a",{false,{26,32}}}]},
                     {record,{26,44},
                      [{"y",{false,{26,47}}},
                       {"x",{record,{26,55},[]}},
                       {"b",{false,{26,60}}}]}}},
                   {eval,{28,1},
                    {timesfloat,{28,1},{float,{28,12},2.0},{float,{28,16},3.14159}}},
                   {eval,{30,1},
                    {ty_abs,{30,1},"X",top,{abs,{30,11},"x",{var,0,1},{var,{30,23},0,2}}}},
                   {eval,{31,2},
                    {ty_app,{31,2},
                     {ty_abs,{31,2},
                      "X",top,
                      {abs,{31,12},"x",{var,0,1},{var,{31,24},0,2}}},
                     {all,"X",top,{arr,{var,0,1},{var,0,1}}}}},
                   {eval,{33,1},
                    {ty_abs,{33,1},
                     "X",
                     {arr,top,top},
                     {abs,{33,21},
                      "x",
                      {var,0,1},
                      {app,{33,33},{var,{33,33},0,2},{var,{33,35},0,2}}}}},
                   {eval,{36,1},{abs,{36,1},"x",bool,{var,{36,16},0,1}}},
                   {eval,{37,2},
                    {app,{37,2},
                     {abs,{37,2},
                      "x",
                      {arr,bool,bool},
                      {if_,{37,23},
                       {app,{37,26},{var,{37,26},0,1},{false,{37,28}}},
                       {true,{37,39}},
                       {false,{37,49}}}},
                     {abs,{38,4},
                      "x",bool,
                      {if_,{38,19},
                       {var,{38,22},0,1},
                       {false,{38,29}},
                       {true,{38,40}}}}}},
                   {eval,{40,1},{abs,{40,1},"x",nat,{succ,{40,15},{var,{40,20},0,1}}}},
                   {eval,{41,2},
                    {app,{41,2},
                     {abs,{41,2},
                      "x",nat,
                      {succ,{41,16},{succ,{41,22},{var,{41,27},0,1}}}},
                     {succ,{41,32},{zero,{41,37}}}}},
                   {bind,{43,1},"T",{ty_abb_bind,{arr,nat,nat}}},
                   {eval,{44,1},
                    {abs,{44,1},
                     "f",
                     {var,0,1},
                     {abs,{44,13},
                      "x",nat,
                      {app,{44,27},
                       {var,{44,27},1,3},
                       {app,{44,30},{var,{44,30},1,3},{var,{44,32},0,3}}}}}},
                   {eval,{47,2},
                    {pack,{47,2},
                     {all,"Y",top,{var,0,2}},
                     {abs,{47,13},"x",{all,"Y",top,{var,0,2}},{var,{47,33},0,2}},
                     {some,"X",top,{arr,{var,0,2},{var,0,2}}}}},
                   {eval,{50,1},
                    {pack,{50,1},
                     nat,
                     {record,{50,8},
                      [{"c",{zero,{50,11}}},
                       {"f",
                        {abs,{50,16},
                         "x",nat,
                         {succ,{50,30},{var,{50,35},0,2}}}}]},
                     {some,"X",top,
                      {record,[{"c",{var,0,2}},{"f",{arr,{var,0,2},nat}}]}}}},
                   {eval,{52,1},
                    {unpack,{52,1},
                     "X","ops",
                     {pack,{52,15},
                      nat,
                      {record,{52,22},
                       [{"c",{zero,{52,25}}},
                        {"f",
                         {abs,{52,30},
                          "x",nat,
                          {succ,{52,44},{var,{52,49},0,2}}}}]},
                      {some,"X",top,
                       {record,[{"c",{var,0,2}},
                                {"f",{arr,{var,0,2},nat}}]}}},
                     {app,{54,8},
                      {proj,{54,8},{var,{54,5},0,3},"f"},
                      {proj,{54,14},{var,{54,11},0,3},"c"}}}}],
                  [{"T",name_bind}]},
                 ParserF([])).
