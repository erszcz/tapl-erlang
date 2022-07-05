Nonterminals
    Top Command Binder Term AppTerm PathTerm ATerm Fields Field
    AType ArrowType AscribeTerm Case Cases FieldType FieldTypes TermSeq TyBinder Type.

Terminals
    inert if then else true false bool case of as lambda let in fix letrec
    ustring unit uunit timesfloat ufloat rec succ pred iszero nat
    ucid lcid int_value float_value string_value
    comma comment dot eq lcurly lparen rcurly rparen semi uscore
    arrow colon ddarrow gt lsquare lt rsquare vbar.


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
Command -> ucid             : fun (Ctx) ->
                                      %% TyBinder is empty
                                      B = binding(ty_var_bind),
                                      Cmd = command({bind, info('$1'), string_value('$1'), B}),
                                      {Cmd, add_name(Ctx, string_value('$1'))}
                              end.
Command -> ucid TyBinder    : fun (Ctx) ->
                                      B = '$2'(Ctx),
                                      Cmd = command({bind, info('$1'), string_value('$1'), B}),
                                      {Cmd, add_name(Ctx, string_value('$1'))}
                              end.
Command -> lcid Binder      : fun (Ctx) ->
                                      Cmd = command({bind, info('$1'), string_value('$1'), '$2'(Ctx)}),
                                      {Cmd, add_name(Ctx, string_value('$1'))}
                              end.


%% Right-hand sides of top-level bindings
Binder -> colon Type    : fun (Ctx) -> binding({var_bind, '$2'(Ctx)}) end.
Binder -> eq Term       : fun (Ctx) -> binding({tm_abb_bind, '$2'(Ctx), none}) end.


%% All type expressions
Type -> ArrowType               : '$1'.
Type -> rec ucid dot Type       : fun (Ctx) ->
                                          NewCtx = add_name(Ctx, string_value('$2')),
                                          ty({rec, string_value('$2'), '$4'(NewCtx)})
                                  end.


%% Atomic types are those that never need extra parentheses
AType -> lparen Type rparen         : '$2'.
AType -> ucid                       : fun ( Ctx) ->
                                          case is_name_bound(Ctx, string_value('$1')) of
                                              true ->
                                                  Index = name_to_index(info('$1'), Ctx, string_value('$1')),
                                                  ty({var, Index, context_length(Ctx)});
                                              false ->
                                                  ty({id, string_value('$1')})
                                          end
                                      end.
AType -> bool                       : fun (_Ctx) -> ty(bool) end.
AType -> lt gt                      : fun (_Ctx) -> ty({variant, []}) end.
AType -> lt FieldTypes gt           : fun ( Ctx) -> ty({variant, '$2'(Ctx, 1)}) end.
AType -> ustring                    : fun (_Ctx) -> ty(string) end.
AType -> uunit                      : fun (_Ctx) -> ty(unit) end.
AType -> lcurly rcurly              : fun (_Ctx) -> ty({record, []}) end.
AType -> lcurly FieldTypes rcurly   : fun ( Ctx) -> ty({record, '$2'(Ctx, 1)}) end.
AType -> ufloat                     : fun (_Ctx) -> ty(float) end.
AType -> nat                        : fun (_Ctx) -> ty(nat) end.

TyBinder -> eq Type                 : fun ( Ctx) -> binding({ty_abb_bind, '$2'(Ctx)}) end.

FieldTypes -> FieldType                     : fun (Ctx, I) -> ['$1'(Ctx, I)] end.
FieldTypes -> FieldType comma FieldTypes    : fun (Ctx, I) -> ['$1'(Ctx, I) | '$3'(Ctx, I+1)] end.

FieldType -> lcid colon Type        : fun (Ctx, _) -> {string_value('$1'), '$3'(Ctx)} end.
FieldType -> Type                   : fun (Ctx, I) -> {integer_to_list(I), '$1'(Ctx)} end.

%% An "arrow type" is a sequence of atomic types separated by arrows
ArrowType -> AType arrow ArrowType  : fun (Ctx) -> ty({arr, '$1'(Ctx), '$3'(Ctx)}) end.
ArrowType -> AType                  : '$1'.

Term -> AppTerm                     : '$1'.
Term -> if Term then Term else Term : fun (Ctx) ->
                                            Info = info('$1'),
                                            term_({if_, Info, '$2'(Ctx), '$4'(Ctx), '$6'(Ctx)})
                                      end.
Term -> case Term of Cases          : fun (Ctx) ->
                                            Info = info('$1'),
                                            term_({case_, Info, '$2'(Ctx), '$4'(Ctx)})
                                      end.
Term -> lambda lcid colon Type dot Term :
        fun (Ctx) ->
            Info = info('$1'),
            NewCtx = add_name(Ctx, string_value('$2')),
            term_({abs, Info, string_value('$2'), '$4'(Ctx), '$6'(NewCtx)})
        end.
Term -> lambda uscore colon Type dot Term :
        fun (Ctx) ->
            Info = info('$1'),
            NewCtx = add_name(Ctx, "_"),
            term_({abs, Info, "_", '$4'(Ctx), '$6'(NewCtx)})
        end.
Term -> let lcid eq Term in Term    : fun (Ctx) ->
                                            X = string_value('$2'),
                                            {let_, info('$1'), X, '$4'(Ctx), '$6'(add_name(Ctx, X))}
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

AppTerm -> PathTerm                     : '$1'.
AppTerm -> AppTerm PathTerm             : fun (Ctx) ->
                                              T1 = '$1'(Ctx),
                                              T2 = '$2'(Ctx),
                                              term_({app, term_info(T1), T1, T2})
                                          end.
AppTerm -> fix PathTerm                 : fun (Ctx) ->
                                              term_({fix, info('$1'), '$2'(Ctx)})
                                          end.
AppTerm -> timesfloat PathTerm PathTerm : fun (Ctx) ->
                                              term_({timesfloat, info('$1'), '$2'(Ctx), '$3'(Ctx)})
                                          end.
AppTerm -> succ PathTerm                : fun (Ctx) -> term_({succ, info('$1'), '$2'(Ctx)}) end.
AppTerm -> pred PathTerm                : fun (Ctx) -> term_({pred, info('$1'), '$2'(Ctx)}) end.
AppTerm -> iszero PathTerm              : fun (Ctx) -> term_({is_zero, info('$1'), '$2'(Ctx)}) end.

AscribeTerm -> ATerm as Type        : fun (Ctx) ->
                                          term_({ascribe, info('$2'), '$1'(Ctx), '$3'(Ctx)})
                                      end.
AscribeTerm -> ATerm                : '$1'.

PathTerm -> PathTerm dot lcid       : fun (Ctx) ->
                                          term_({proj, info('$2'), '$1'(Ctx), string_value('$3')})
                                      end.
PathTerm -> PathTerm dot int_value  : fun (Ctx) ->
                                          Label = integer_to_list(int_value('$3')),
                                          term_({proj, info('$2'), '$1'(Ctx), Label})
                                      end.
PathTerm -> AscribeTerm             : '$1'.

TermSeq -> Term                     : '$1'.
TermSeq -> Term semi TermSeq        : fun (Ctx) ->
                                          Info = info('$2'),
                                          NewCtx = add_name(Ctx, "_"),
                                          Abs = term_({abs, Info, "_", ty(unit), '$3'(NewCtx)}),
                                          term_({app, Info, Abs, '$1'(Ctx)})
                                      end.

%% Atomic terms are ones that never require extra parentheses
ATerm -> lparen TermSeq rparen      : '$2'.
ATerm -> inert lsquare Type rsquare : fun ( Ctx) -> term_({inert, info('$1'), '$3'(Ctx)}) end.
ATerm -> true                       : fun (_Ctx) -> term_({true, info('$1')}) end.
ATerm -> false                      : fun (_Ctx) -> term_({false, info('$1')}) end.
ATerm -> lt lcid eq Term gt as Type :
         fun ( Ctx) ->
                 term_({tag, info('$1'), string_value('$2'), '$4'(Ctx), '$7'(Ctx)})
         end.
ATerm -> lcid                       : fun ( Ctx) ->
                                            Index = name_to_index(info('$1'), Ctx, string_value('$1')),
                                            term_({var, info('$1'), Index, context_length(Ctx)})
                                      end.
ATerm -> string_value               : fun (_Ctx) -> term_({string, info('$1'), string_value('$1')}) end.
ATerm -> unit                       : fun (_Ctx) -> term_({unit, info('$1')}) end.
ATerm -> lcurly rcurly              : fun (_Ctx) -> term_({record, info('$1'), []}) end.
ATerm -> lcurly Fields rcurly       : fun ( Ctx) -> term_({record, info('$1'), '$2'(Ctx, 1)}) end.
ATerm -> float_value                : fun (_Ctx) -> term_({float, info('$1'), float_value('$1')}) end.
ATerm -> int_value                  : fun (_Ctx) -> term_(int_value_term('$1')) end.

Cases -> Case                   : fun (Ctx) -> ['$1'(Ctx)] end.
Cases -> Case vbar Cases        : fun (Ctx) -> ['$1'(Ctx) | '$3'(Ctx)] end.

Case -> lt lcid eq lcid gt ddarrow AppTerm :
        fun (Ctx) ->
            NewCtx = add_name(Ctx, string_value('$4')),
            {string_value('$2'), {string_value('$4'), '$7'(NewCtx)}}
        end.

Fields -> Field                 : fun (Ctx, I) -> ['$1'(Ctx, I)] end.
Fields -> Field comma Fields    : fun (Ctx, I) -> ['$1'(Ctx, I) | '$3'(Ctx, I+1)] end.

Field -> lcid eq Term   : fun (Ctx, _) -> {string_value('$1'), '$3'(Ctx)} end.
Field -> Term           : fun (Ctx, I) -> {integer_to_list(I), '$1'(Ctx)} end.

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

int_value({int_value, _Info, S}) when is_list(S) -> list_to_integer(S).

int_value_term({int_value, Info, S}) when is_list(S) -> int_value(list_to_integer(S), Info).

int_value(0, Info) -> {zero, Info};
int_value(N, Info) when N > 0 -> term_({succ, Info, int_value(N - 1, Info)}).

float_value({float_value, _Info, Chars}) -> list_to_float(Chars).

string_value({lcid, _Info, Chars}) -> Chars;
string_value({ucid, _Info, Chars}) -> Chars;
string_value({string_value, _Info, Chars}) -> string:trim(Chars, both, [$"]).
