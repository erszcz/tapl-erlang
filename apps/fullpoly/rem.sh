#!/usr/bin/env sh

erl -remsh $(epmd -names | awk '/fullrecon/ { print $2 }') -sname remote

