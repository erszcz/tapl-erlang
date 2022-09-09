f().
r3:compile().
Input = "test.f",
Data = unicode:characters_to_list(element(2, file:read_file(Input))),
fullrecon_lexer:reset(),
{ok, Tokens, _} = fullrecon_lexer:string(Data),
{ok, Parser} = fullrecon_parser:parse(Tokens).
dbg:stop_clear(),
dbg:tracer(process, {fun (Trace, ok) -> io:format("~p\n", [Trace]) end, ok}),
%dbg:p(all, call),
dbg:tpl(fullrecon_syntax, x).
dbg:tpl(fullrecon_parser, x).
rp(Parser([])).
