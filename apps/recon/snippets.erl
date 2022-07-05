f().
r3:compile().
Input = "test.f",
Data = unicode:characters_to_list(element(2, file:read_file(Input))),
recon_lexer:reset(),
{ok, Tokens, _} = recon_lexer:string(Data),
{ok, Parser} = recon_parser:parse(Tokens).
dbg:stop_clear(),
dbg:tracer(process, {fun (Trace, ok) -> io:format("~p\n", [Trace]) end, ok}),
%dbg:p(all, call),
dbg:tpl(recon_syntax, x).
dbg:tpl(recon_parser, x).
rp(Parser([])).
