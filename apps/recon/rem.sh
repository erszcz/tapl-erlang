#!/usr/bin/env sh

erl -remsh $(epmd -names | awk '/recon/ { print $2 }') -sname remote

