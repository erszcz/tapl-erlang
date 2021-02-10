Nonterminals
    Top Command Term AppTerm ATerm.

Terminals
    if then else true false succ pred iszero int_value
    comment semi lparen rparen.

Rootsymbol Top.

Top -> Command semi : ['$1'].
Top -> Command semi Top : ['$1' | '$3'].
Top -> comment Top : '$2'.

Command -> Term : {eval, info, '$1'}.

Term -> AppTerm : '$1'.
Term -> if Term then Term else Term : {'if', info, '$2', '$4', '$6'}.

AppTerm -> ATerm : '$1'.
AppTerm -> succ ATerm : {succ, info, '$2'}.
AppTerm -> pred ATerm : {pred, info, '$2'}.
AppTerm -> iszero ATerm : {is_zero, info, '$2'}.

%% Atomic terms are ones that never require extra parentheses
ATerm -> lparen Term rparen : '$2'.
ATerm -> true : {true, info}.
ATerm -> false : {false, info}.
ATerm -> int_value : int_value('$1').

Erlang code.

int_value({int_value, _, S}) when is_list(S) -> int_value(list_to_integer(S));
int_value(0) -> {zero, info};
int_value(N) when N > 0 -> {succ, info, int_value(N - 1)}.
