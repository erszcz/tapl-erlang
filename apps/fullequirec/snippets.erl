f().
r3:compile().
Input = "test.f",
Data = unicode:characters_to_list(element(2, file:read_file(Input))),
fullsimple_lexer:reset(),
{ok, Tokens, _} = fullsimple_lexer:string(Data),
{ok, Parser} = fullsimple_parser:parse(Tokens).
dbg:stop_clear(),
dbg:tracer(process, {fun (Trace, ok) -> io:format("~p\n", [Trace]) end, ok}),
%dbg:p(all, call),
dbg:tpl(fullsimple_syntax, x).
dbg:tpl(fullsimple_parser, x).
rp(Parser([])).
