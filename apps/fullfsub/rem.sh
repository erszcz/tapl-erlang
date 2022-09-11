#!/usr/bin/env sh

erl -remsh $(epmd -names | awk '/fullfsub/ { print $2 }') -sname remote

