Nonterminals
    Top Command Binder Term AppTerm PathTerm ATerm Fields Field.

Terminals
    if then else true false lambda timesfloat succ pred iszero let in
    ucid lcid int_value float_value string_value
    comma comment dot eq lcurly lparen rcurly rparen semi slash uscore.

Rootsymbol Top.

Top -> Command semi     : fun (Ctx) ->
                                  {Cmd, Ctx1} = '$1'(Ctx),
                                  {[Cmd], Ctx1}
                          end.
Top -> Command semi Top : fun (Ctx) ->
                                  {Cmd, Ctx1} = '$1'(Ctx),
                                  {Cmds, Ctx2} = '$3'(Ctx1),
                                  {[Cmd | Cmds], Ctx2}
                          end.
Top -> comment Top      : '$2'.

Command -> Term         : fun (Ctx) ->
                                  T = '$1'(Ctx),
                                  {command({eval, term_info(T), T}), Ctx}
                          end.
Command -> lcid Binder  : fun (Ctx) ->
                                  Cmd = command({bind, info('$1'), string_value('$1'), '$2'(Ctx)}),
                                  {Cmd, add_name(Ctx, string_value('$1'))}
                          end.

Binder -> slash     : fun (_Ctx) -> binding(name_bind) end.
Binder -> eq Term   : fun ( Ctx) -> binding({abb_bind, '$2'(Ctx)}) end.


Term -> AppTerm                     : '$1'.
Term -> if Term then Term else Term : fun (Ctx) ->
                                            Info = info('$1'),
                                            term_({if_, Info, '$2'(Ctx), '$4'(Ctx), '$6'(Ctx)})
                                      end.
Term -> lambda lcid dot Term        : fun (Ctx) ->
                                            Info = info('$1'),
                                            NewCtx = add_name(Ctx, string_value('$2')),
                                            term_({abs, Info, string_value('$2'), '$4'(NewCtx)})
                                      end.
Term -> lambda uscore dot Term      : fun (Ctx) ->
                                            Info = info('$1'),
                                            NewCtx = add_name(Ctx, "_"),
                                            term_({abs, Info, "_", '$4'(NewCtx)})
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
                                              term_({app, term_info(T1), T1, T2})
                                          end.
AppTerm -> timesfloat PathTerm PathTerm : fun (Ctx) ->
                                              term_({timesfloat, info('$1'), '$2'(Ctx), '$3'(Ctx)})
                                          end.
AppTerm -> succ PathTerm                : fun (Ctx) -> term_({succ, info('$1'), '$2'(Ctx)}) end.
AppTerm -> pred PathTerm                : fun (Ctx) -> term_({pred, info('$1'), '$2'(Ctx)}) end.
AppTerm -> iszero PathTerm              : fun (Ctx) -> term_({is_zero, info('$1'), '$2'(Ctx)}) end.

PathTerm -> PathTerm dot lcid       : fun (Ctx) ->
                                          term_({proj, info('$2'), '$1'(Ctx), string_value('$3')})
                                      end.
PathTerm -> PathTerm dot int_value  : fun (Ctx) ->
                                          Label = integer_to_list(int_value('$3')),
                                          term_({proj, info('$2'), '$1'(Ctx), Label})
                                      end.
PathTerm -> ATerm                   : '$1'.

%% Atomic terms are ones that never require extra parentheses
ATerm -> lparen Term rparen     : '$2'.
ATerm -> true                   : fun (_Ctx) -> term_({true, info('$1')}) end.
ATerm -> false                  : fun (_Ctx) -> term_({false, info('$1')}) end.
ATerm -> lcid                   : fun ( Ctx) ->
                                        Index = name_to_index(info('$1'), Ctx, string_value('$1')),
                                        term_({var, info('$1'), Index, context_length(Ctx)})
                                  end.
ATerm -> lcurly rcurly          : fun (_Ctx) -> term_({record, info('$1'), []}) end.
ATerm -> lcurly Fields rcurly   : fun ( Ctx) -> term_({record, info('$1'), '$2'(Ctx, 1)}) end.
ATerm -> float_value            : fun (_Ctx) -> term_({float, info('$1'), float_value('$1')}) end.
ATerm -> string_value           : fun (_Ctx) -> term_({string, info('$1'), string_value('$1')}) end.
ATerm -> int_value              : fun (_Ctx) -> term_(int_value('$1')) end.

Fields -> Field                 : fun (Ctx, I) -> ['$1'(Ctx, I)] end.
Fields -> Field comma Fields    : fun (Ctx, I) -> ['$1'(Ctx, I) | '$3'(Ctx, I+1)] end.

Field -> lcid eq Term   : fun (Ctx, _) -> {string_value('$1'), '$3'(Ctx)} end.
Field -> Term           : fun (Ctx, I) -> {integer_to_list(I), '$1'(Ctx)} end.

Erlang code.

-import(fullsimple_syntax, [binding/1,
                            command/1,
                            info/1,
                            term_/1]).

-import(fullsimple_syntax, [add_name/2,
                            context_length/1,
                            name_to_index/3,
                            term_info/1]).

int_value({int_value, Info, S}) when is_list(S) -> int_value(list_to_integer(S), Info).

int_value(0, Info) -> {zero, Info};
int_value(N, Info) when N > 0 -> term_({succ, Info, int_value(N - 1, Info)}).

float_value({float_value, _Info, Chars}) -> list_to_float(Chars).

string_value({lcid, _Info, Chars}) -> Chars;
string_value({string_value, _Info, Chars}) -> string:trim(Chars, both, [$"]).
