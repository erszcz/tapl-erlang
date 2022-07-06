#!/usr/bin/env bash
# vim: sts=2 ts=2 sw=2 et

set -u
echo $1 >/dev/null
INPUT=$1

read CODE << EOF
Input = "$INPUT", \
Data = unicode:characters_to_list(element(2, file:read_file(Input))), \
fullequirec_lexer:reset(), \
Lexed = fullequirec_lexer:string(Data), \
io:format("~p\\\\n", [Lexed]), \
halt().
EOF

erl -pa _build/default/lib/fullequirec/ebin -noinput -noshell -eval "$CODE"
