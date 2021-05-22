# 2021-05-22

I've applied Gradualizer on `arith_syntax` and after tweaking it a little
bit (see https://github.com/erszcz/Gradualizer/tree/exhaustive-user-type)
I managed to make it detect nonexhaustive pattern matching.

However, it doesn't yet seem to show nonexhaustiveness issues with the
following diff on `arith_core`:

```
diff --git a/apps/arith/src/arith_core.erl b/apps/arith/src/arith_core.erl
index 9111dab..656a9d5 100644
--- a/apps/arith/src/arith_core.erl
+++ b/apps/arith/src/arith_core.erl
@@ -18,13 +18,6 @@ eval(T) ->
 -spec eval1(term_()) -> term_().
 eval1(T) ->
     case T of
-        {'if', _, {true, _}, T2, _} ->
-            T2;
-        {'if', _, {false, _}, _, T3} ->
-            T3;
-        {'if', Info, T1, T2, T3} ->
-            T1_ = eval1(T1),
-            {'if', Info, T1_, T2, T3};
         {succ, Info, T1} ->
             T1_ = eval1(T1),
             {succ, Info, T1_};
@@ -41,9 +34,7 @@ eval1(T) ->
             {false, 0};
         {is_zero, Info, T1} ->
             T1_ = eval1(T1),
-            {is_zero, Info, T1_};
-        _ ->
-            erlang:throw(no_rule_applies)
+            {is_zero, Info, T1_}
     end.
 
 -spec trace() -> ok.
```

Why doesn't it kick in?!

Answer: it does, when we do this:

```
diff --git a/apps/arith/src/arith_core.erl b/apps/arith/src/arith_core.erl
index 9111dab..4c1a5a9 100644
--- a/apps/arith/src/arith_core.erl
+++ b/apps/arith/src/arith_core.erl
@@ -5,7 +5,17 @@
 -export([eval/1,
          trace/0]).
 
--type term_() :: arith_syntax:term_().
+%-type term_() :: arith_syntax:term_().
+
+-type info() :: integer().
+
+-type term_() :: {true, info()}
+               | {false, info()}
+               | {'if', info(), term_(), term_(), term_()}
+               | {zero, info()}
+               | {succ, info(), term_()}
+               | {pred, info(), term_()}
+               | {is_zero, info(), term_()}.
 
 -spec eval(term_()) -> term_().
 eval(T) ->
```

```
$ gradualizer -pa _build/default/lib/arith/ebin -- src/arith_core.erl
src/arith_core.erl: Nonexhaustive patterns on line 31 at column 9
Example values which are not covered:
	{false, 0}
```
