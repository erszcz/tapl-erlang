Nonterminals
    Top Command Binder Term AppTerm ATerm AType ArrowType Type.

Terminals
    if then else true false bool lambda let in succ pred iszero nat
    ucid lcid int_value comment dot eq lparen rparen semi uscore arrow colon.


%% The top level of a file is a sequence of commands, each terminated by a semicolon.
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


%% A top-level command
Command -> Term         : fun (Ctx) ->
                                  T = '$1'(Ctx),
                                  {command({eval, term_info(T), T}), Ctx}
                          end.
Command -> lcid Binder  : fun (Ctx) ->
                                  Cmd = command({bind, info('$1'), string_value('$1'), '$2'(Ctx)}),
                                  {Cmd, add_name(Ctx, string_value('$1'))}
                          end.


%% Right-hand sides of top-level bindings
Binder -> colon Type    : fun (Ctx) -> binding({var_bind, '$2'(Ctx)}) end.


%% All type expressions
Type -> ArrowType       : '$1'.


%% Atomic types are those that never need extra parentheses
AType -> lparen Type rparen         : '$2'.
AType -> bool                       : fun (_Ctx) -> ty(bool) end.
AType -> nat                        : fun (_Ctx) -> ty(nat) end.
AType -> ucid                       : fun (_Ctx) -> ty({id, string_value('$1')}) end.

%% An "arrow type" is a sequence of atomic types separated by arrows
ArrowType -> AType arrow ArrowType  : fun (Ctx) -> ty({arr, '$1'(Ctx), '$3'(Ctx)}) end.
ArrowType -> AType                  : '$1'.

Term -> AppTerm                     : '$1'.
Term -> let lcid eq Term in Term    : fun (Ctx) ->
                                            X = string_value('$2'),
                                            {let_, info('$1'), X, '$4'(Ctx), '$6'(add_name(Ctx, X))}
                                      end.
Term -> let uscore eq Term in Term  : fun (Ctx) ->
                                            X = "_",
                                            {let_, info('$1'), X, '$4'(Ctx), '$6'(add_name(Ctx, X))}
                                      end.
Term -> if Term then Term else Term : fun (Ctx) ->
                                            Info = info('$1'),
                                            term_({if_, Info, '$2'(Ctx), '$4'(Ctx), '$6'(Ctx)})
                                      end.
Term -> lambda lcid dot Term :
        fun (Ctx) ->
            Info = info('$1'),
            NewCtx = add_name(Ctx, string_value('$2')),
            term_({abs, Info, string_value('$2'), none, '$4'(NewCtx)})
        end.
Term -> lambda lcid colon Type dot Term :
        fun (Ctx) ->
            Info = info('$1'),
            NewCtx = add_name(Ctx, string_value('$2')),
            term_({abs, Info, string_value('$2'), {ok, '$4'(Ctx)}, '$6'(NewCtx)})
        end.
Term -> lambda uscore colon Type dot Term :
        fun (Ctx) ->
            Info = info('$1'),
            NewCtx = add_name(Ctx, "_"),
            term_({abs, Info, "_", {ok, '$4'(Ctx)}, '$6'(NewCtx)})
        end.

AppTerm -> ATerm                 : '$1'.
AppTerm -> succ ATerm            : fun (Ctx) -> term_({succ, info('$1'), '$2'(Ctx)}) end.
AppTerm -> pred ATerm            : fun (Ctx) -> term_({pred, info('$1'), '$2'(Ctx)}) end.
AppTerm -> iszero ATerm          : fun (Ctx) -> term_({is_zero, info('$1'), '$2'(Ctx)}) end.
AppTerm -> AppTerm ATerm         : fun (Ctx) ->
                                       T1 = '$1'(Ctx),
                                       T2 = '$2'(Ctx),
                                       term_({app, term_info(T1), T1, T2})
                                   end.

%% Atomic terms are ones that never require extra parentheses
ATerm -> lparen Term rparen      : '$2'.
ATerm -> lcid                    : fun ( Ctx) ->
                                         Index = name_to_index(info('$1'), Ctx, string_value('$1')),
                                         term_({var, info('$1'), Index, context_length(Ctx)})
                                   end.
ATerm -> true                    : fun (_Ctx) -> term_({true, info('$1')}) end.
ATerm -> false                   : fun (_Ctx) -> term_({false, info('$1')}) end.
ATerm -> int_value               : fun (_Ctx) -> term_(int_value_term('$1')) end.

Erlang code.

-import(fullrecon_syntax, [ty/1,
                           binding/1,
                           command/1,
                           info/1,
                           term_/1]).

-import(fullrecon_syntax, [add_name/2,
                           is_name_bound/2,
                           context_length/1,
                           name_to_index/3,
                           term_info/1]).

int_value_term({int_value, Info, S}) when is_list(S) -> int_value(list_to_integer(S), Info).

int_value(0, Info) -> {zero, Info};
int_value(N, Info) when N > 0 -> term_({succ, Info, int_value(N - 1, Info)}).

string_value({lcid, _Info, Chars}) -> Chars;
string_value({ucid, _Info, Chars}) -> Chars;
string_value({string_value, _Info, Chars}) -> string:trim(Chars, both, [$"]).
