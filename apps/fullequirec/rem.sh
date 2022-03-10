#!/usr/bin/env sh

erl -remsh $(epmd -names | awk '/fullequirec/ { print $2 }') -sname remote

