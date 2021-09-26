f().
r3:compile().
Input = "test.f",
Data = unicode:characters_to_list(element(2, file:read_file(Input))),
fulluntyped_lexer:reset(),
{ok, Tokens, _} = fulluntyped_lexer:string(Data),
{ok, Parser} = fulluntyped_parser:parse(Tokens).
dbg:stop_clear(),
dbg:tracer(process, {fun (Trace, ok) -> io:format("~p\n", [Trace]) end, ok}),
%dbg:p(all, call),
dbg:tpl(fulluntyped_syntax, x).
dbg:tpl(fulluntyped_parser, x).
rp(Parser([])).
