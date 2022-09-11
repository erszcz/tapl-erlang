#!/usr/bin/env sh

erl -remsh $(epmd -names | awk '/fullpoly/ { print $2 }') -sname remote

