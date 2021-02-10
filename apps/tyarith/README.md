# tyarith

```
$ rebar3 escriptize
$ cat test/test.f
/* 1 or 2 examples for testing */

true;
if false then true else false;

/* another comment */

0;
succ (pred 0);
iszero (pred (succ (succ 0)));
$ ./_build/default/bin/tyarith test/test.f
true
false
0
1
false
```
