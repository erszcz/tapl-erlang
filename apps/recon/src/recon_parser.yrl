Nonterminals
    Top Command Binder Term AppTerm ATerm
    AType ArrowType Type.

Terminals
    if then else true false bool let in letrec
    succ pred iszero nat
    lcid int_value
    comment eq lparen rparen semi uscore
    arrow colon.


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
Command -> Term             : fun (Ctx) ->
                                      T = '$1'(Ctx),
                                      {command({eval, term_info(T), T}), Ctx}
                              end.
Command -> lcid Binder      : fun (Ctx) ->
                                      Cmd = command({bind, info('$1'), string_value('$1'), '$2'(Ctx)}),
                                      {Cmd, add_name(Ctx, string_value('$1'))}
                              end.


%% Right-hand sides of top-level bindings
Binder -> colon Type    : fun (Ctx) -> binding({var_bind, '$2'(Ctx)}) end.


%% All type expressions
Type -> ArrowType               : '$1'.


%% Atomic types are those that never need extra parentheses
AType -> lparen Type rparen         : '$2'.
AType -> bool                       : fun (_Ctx) -> ty(bool) end.
AType -> nat                        : fun (_Ctx) -> ty(nat) end.

%% An "arrow type" is a sequence of atomic types separated by arrows
ArrowType -> AType arrow ArrowType  : fun (Ctx) -> ty({arr, '$1'(Ctx), '$3'(Ctx)}) end.
ArrowType -> AType                  : '$1'.

Term -> AppTerm                     : '$1'.
Term -> if Term then Term else Term : fun (Ctx) ->
                                            Info = info('$1'),
                                            term_({if_, Info, '$2'(Ctx), '$4'(Ctx), '$6'(Ctx)})
                                      end.
Term -> let uscore eq Term in Term  : fun (Ctx) ->
                                            X = "_",
                                            {let_, info('$1'), X, '$4'(Ctx), '$6'(add_name(Ctx, X))}
                                      end.
Term -> letrec lcid colon Type eq Term in Term :
        fun (Ctx) ->
            Info = info('$1'),
            NewCtx = add_name(Ctx, string_value('$2')),
            Abs = term_({abs, Info, string_value('$2'), '$4'(Ctx), '$6'(NewCtx)}),
            Fix = term_({fix, Info, Abs}),
            term_({let_, Info, string_value('$2'), Fix, '$8'(NewCtx)})
        end.

AppTerm -> ATerm                     : '$1'.
AppTerm -> succ ATerm                : fun (Ctx) -> term_({succ, info('$1'), '$2'(Ctx)}) end.
AppTerm -> pred ATerm                : fun (Ctx) -> term_({pred, info('$1'), '$2'(Ctx)}) end.
AppTerm -> iszero ATerm              : fun (Ctx) -> term_({is_zero, info('$1'), '$2'(Ctx)}) end.

%% Atomic terms are ones that never require extra parentheses
ATerm -> true                       : fun (_Ctx) -> term_({true, info('$1')}) end.
ATerm -> false                      : fun (_Ctx) -> term_({false, info('$1')}) end.
ATerm -> int_value                  : fun (_Ctx) -> term_(int_value_term('$1')) end.
ATerm -> lcid                       : fun ( Ctx) ->
                                            Index = name_to_index(info('$1'), Ctx, string_value('$1')),
                                            term_({var, info('$1'), Index, context_length(Ctx)})
                                      end.

Erlang code.

-import(recon_syntax, [ty/1,
                       binding/1,
                       command/1,
                       info/1,
                       term_/1]).

-import(recon_syntax, [add_name/2,
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
