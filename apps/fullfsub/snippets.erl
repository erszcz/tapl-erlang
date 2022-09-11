f().
r3:compile().
Input = "test.f",
Data = unicode:characters_to_list(element(2, file:read_file(Input))),
fullpoly_lexer:reset(),
{ok, Tokens, _} = fullpoly_lexer:string(Data),
{ok, Parser} = fullpoly_parser:parse(Tokens).
dbg:stop_clear(),
dbg:tracer(process, {fun (Trace, ok) -> io:format("~p\n", [Trace]) end, ok}),
%dbg:p(all, call),
dbg:tpl(fullpoly_syntax, x).
dbg:tpl(fullpoly_parser, x).
rp(Parser([])).
