Nonterminals
    Top Command Term AppTerm ATerm.

Terminals
    if then else true false succ pred iszero int_value
    comment semi lparen rparen.

Rootsymbol Top.

Top -> Command semi : ['$1'].
Top -> Command semi Top : ['$1' | '$3'].
Top -> comment Top : '$2'.

Command -> Term : fulluntyped_syntax:eval('$1').

Term -> AppTerm : '$1'.
Term -> if Term then Term else Term : fulluntyped_syntax:'if'('$1', '$2', '$4', '$6').

AppTerm -> ATerm : '$1'.
AppTerm -> succ ATerm : fulluntyped_syntax:succ('$1', '$2').
AppTerm -> pred ATerm : fulluntyped_syntax:pred('$1', '$2').
AppTerm -> iszero ATerm : fulluntyped_syntax:is_zero('$1', '$2').

%% Atomic terms are ones that never require extra parentheses
ATerm -> lparen Term rparen : '$2'.
ATerm -> true : '$1'.
ATerm -> false : '$1'.
ATerm -> int_value : int_value('$1').

Erlang code.

int_value({int_value, Info, S}) when is_list(S) -> int_value(list_to_integer(S), Info).

int_value(0, Info) -> {zero, Info};
int_value(N, Info) when N > 0 -> fulluntyped_syntax:succ({succ, Info}, int_value(N - 1, Info)).
