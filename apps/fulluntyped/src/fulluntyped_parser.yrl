Nonterminals
    Top Command Binder Term AppTerm PathTerm ATerm Fields Field.

Terminals
    if then else true false lambda timesfloat succ pred iszero let in
    ucid lcid int_value float_value string_value
    comma comment dot eq lcurly lparen rcurly rparen semi slash uscore.

Rootsymbol Top.

Top -> Command semi     : fun (Ctx) -> {Cmd, Ctx1} = '$1'(Ctx), {[Cmd], Ctx1} end.
Top -> Command semi Top : fun (Ctx) ->
                                  {Cmd, Ctx1} = '$1'(Ctx),
                                  {Cmds, Ctx2} = '$3'(Ctx1),
                                  {[Cmd | Cmds], Ctx2}
                          end.
Top -> comment Top      : '$2'.

Command -> Term         : fun (Ctx) -> T = '$1'(Ctx), {eval(T), Ctx} end.
Command -> lcid Binder  : fun (Ctx) ->
                              { {bind, info('$1'), string_value('$1'), '$2'(Ctx)},
                                add_name(Ctx, string_value('$1')) }
                          end.

Binder -> slash     : fun (_Ctx) -> name_bind end.
Binder -> eq Term   : fun (Ctx) -> {abb_bind, '$2'(Ctx)} end.


Term -> AppTerm                     : '$1'.
Term -> if Term then Term else Term : fun (Ctx) -> if_('$1', '$2'(Ctx), '$4'(Ctx), '$6'(Ctx)) end.
Term -> lambda lcid dot Term        : fun (Ctx) ->
                                            NewCtx = add_name(Ctx, string_value('$2')),
                                            abs_(string_value('$2'), '$4'(NewCtx))
                                      end.
Term -> lambda uscore dot Term      : fun (Ctx) ->
                                            NewCtx = add_name(Ctx, "_"),
                                            abs_("_", '$4'(NewCtx))
                                      end.
Term -> let lcid eq Term in Term    : fun (Ctx) ->
                                            X = string_value('$2'),
                                            {let_, info('$1'), X, '$4'(Ctx), '$6'(add_name(Ctx, X))}
                                      end.
Term -> let uscore eq Term in Term  : fun (Ctx) ->
                                            X = "_",
                                            {let_, info('$1'), X, '$4'(Ctx), '$6'(add_name(Ctx, X))}
                                      end.

AppTerm -> PathTerm                     : '$1'.
AppTerm -> AppTerm PathTerm             : fun (Ctx) ->
                                                  T1 = '$1'(Ctx),
                                                  T2 = '$2'(Ctx),
                                                  app(T1, T2)
                                          end.
AppTerm -> timesfloat PathTerm PathTerm : fun (Ctx) ->
                                                  {timesfloat, info('$1'), '$2'(Ctx), '$3'(Ctx)}
                                          end.
AppTerm -> succ PathTerm                : fun (Ctx) -> succ('$1', '$2'(Ctx)) end.
AppTerm -> pred PathTerm                : fun (Ctx) -> pred('$1', '$2'(Ctx)) end.
AppTerm -> iszero PathTerm              : fun (Ctx) -> is_zero('$1', '$2'(Ctx)) end.

PathTerm -> PathTerm dot lcid       : fun (Ctx) ->
                                            {proj, info('$2'), '$1'(Ctx), string_value('$3')}
                                      end.
PathTerm -> PathTerm dot int_value  : fun (Ctx) ->
                                            {proj, info('$2'), '$1'(Ctx),
                                             integer_to_list(int_value('$3'))}
                                      end.
PathTerm -> ATerm                   : '$1'.

%% Atomic terms are ones that never require extra parentheses
ATerm -> lparen Term rparen     : '$2'.
ATerm -> true                   : fun (_Ctx) -> {true, info('$1')} end.
ATerm -> false                  : fun (_Ctx) -> {false, info('$1')} end.
ATerm -> lcid                   : fun (Ctx) ->
                                        Index = name_to_index(info('$1'), Ctx, string_value('$1')),
                                        {var, info('$1'), Index, context_length(Ctx)}
                                  end.
ATerm -> lcurly rcurly          : fun (Ctx) -> {record, info('$1'), []} end.
ATerm -> lcurly Fields rcurly   : fun (Ctx) -> {record, info('$1'), '$2'(Ctx, 1)} end.
ATerm -> float_value            : fun (Ctx) -> {float, info('$1'), float_value('$1')} end.
ATerm -> string_value           : fun (Ctx) -> {string, info('$1'), string_value('$1')} end.
ATerm -> int_value              : fun (_Ctx) -> int_value('$1') end.

Fields -> Field                 : fun (Ctx, I) -> ['$1'(Ctx, I)] end.
Fields -> Field comma Fields    : fun (Ctx, I) -> ['$1'(Ctx, I) | '$3'(Ctx, I+1)] end.

Field -> lcid eq Term   : fun (Ctx, I) -> {string_value('$1'), '$3'(Ctx)} end.
Field -> Term           : fun (Ctx, I) -> {integer_to_list(I), '$1'(Ctx)} end.

Erlang code.

-import(fulluntyped_syntax, [add_name/2,
                             bind/2,
                             context_length/1,
                             eval/1,
                             abs_/2,
                             app/2,
                             if_/4,
                             proj/3,
                             succ/2,
                             pred/2,
                             is_zero/2,
                             name_to_index/3]).

int_value({int_value, Info, S}) when is_list(S) -> int_value(list_to_integer(S), Info).

int_value(0, Info) -> {zero, Info};
int_value(N, Info) when N > 0 -> fulluntyped_syntax:succ({succ, Info}, int_value(N - 1, Info)).

float_value({float_value, _Info, Chars}) -> list_to_float(Chars).

string_value({lcid, _Info, Chars}) -> Chars;
string_value({string_value, _Info, Chars}) -> string:trim(Chars, both, [$"]).

info({_, Info}) -> Info;
info({_, Info, _Chars}) -> Info.
