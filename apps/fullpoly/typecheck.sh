#!/usr/bin/env sh

find src -name \*.erl | grep -v _parser.erl | xargs gradualizer -pa _build/default/lib/*/ebin --

