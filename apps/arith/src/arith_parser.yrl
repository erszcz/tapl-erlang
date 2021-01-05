Nonterminals
    Top Comment Command Term AppTerm ATerm
    Commented Commented1.

Terminals
    semi lcomment rcomment lparen rparen if then else true false succ pred
    iszero int_value lcid ucid.

Rootsymbol Top.

Top -> Command semi : ['$1'].
Top -> Command semi Top : ['$1' | '$3'].
Top -> Comment Top : '$2'.

%% Nested comments are not handled yet
Comment -> lcomment rcomment.
Comment -> lcomment Commented rcomment.

Commented -> Commented1.
Commented -> Commented1 Commented.

Commented1 -> semi.
Commented1 -> lparen.
Commented1 -> rparen.
Commented1 -> if.
Commented1 -> then.
Commented1 -> else.
Commented1 -> true.
Commented1 -> false.
Commented1 -> succ.
Commented1 -> pred.
Commented1 -> iszero.
Commented1 -> int_value.
Commented1 -> lcid.
Commented1 -> ucid.

Command -> Term : {eval, info, '$1'}.

Term -> AppTerm : '$1'.
Term -> if Term then Term else Term : {'if', info, '$2', '$4', '$6'}.

AppTerm -> ATerm : '$1'.
AppTerm -> succ ATerm : {succ, info, '$2'}.
AppTerm -> pred ATerm : {pred, info, '$2'}.
AppTerm -> iszero ATerm : {is_zero, info, '$2'}.

%% Atomic terms are ones that never require extra parentheses
ATerm -> lparen Term rparen : '$2'.
ATerm -> true : {true, info, '$1'}.
ATerm -> false : {false, info, '$1'}.
ATerm -> int_value : int_value('$1').

Erlang code.

int_value({int_value, _, S}) when is_list(S) -> int_value(list_to_integer(S));
int_value(0) -> {zero, info};
int_value(N) when N > 0 -> {succ, info, int_value(N - 1)}.
