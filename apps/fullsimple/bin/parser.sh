#!/usr/bin/env bash
# vim: sts=2 ts=2 sw=2 et

set -u
echo $1 >/dev/null
INPUT=$1

read CODE << EOF
Data = unicode:characters_to_list(element(2, file:read_file("$INPUT"))), \
fullsimple_lexer:reset(), \
{ok, Tokens, _} = fullsimple_lexer:string(Data), \
{ok, Parser} = fullsimple_parser:parse(Tokens), \
EmptyCtx = [], \
io:format("~p\\\\n", [Parser(EmptyCtx)]), \
halt().
EOF

erl -pa _build/default/lib/fullsimple/ebin -noinput -noshell -eval "$CODE"
