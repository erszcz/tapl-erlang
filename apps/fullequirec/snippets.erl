f().
r3:compile().
Input = "test.f",
Data = unicode:characters_to_list(element(2, file:read_file(Input))),
fullequirec_lexer:reset(),
{ok, Tokens, _} = fullequirec_lexer:string(Data),
{ok, Parser} = fullequirec_parser:parse(Tokens).
dbg:stop_clear(),
dbg:tracer(process, {fun (Trace, ok) -> io:format("~p\n", [Trace]) end, ok}),
%dbg:p(all, call),
dbg:tpl(fullequirec_syntax, x).
dbg:tpl(fullequirec_parser, x).
rp(Parser([])).
