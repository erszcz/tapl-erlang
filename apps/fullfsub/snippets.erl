f().
r3:compile().
Input = "test.f",
Data = unicode:characters_to_list(element(2, file:read_file(Input))),
fullfsub_lexer:reset(),
{ok, Tokens, _} = fullfsub_lexer:string(Data),
{ok, Parser} = fullfsub_parser:parse(Tokens).
dbg:stop_clear(),
dbg:tracer(process, {fun (Trace, ok) -> io:format("~p\n", [Trace]) end, ok}),
%dbg:p(all, call),
dbg:tpl(fullfsub_syntax, x).
dbg:tpl(fullfsub_parser, x).
rp(Parser([])).
