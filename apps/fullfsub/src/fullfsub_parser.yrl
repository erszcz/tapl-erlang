Nonterminals
    Top Command Binder Term AppTerm PathTerm ATerm Fields Field
    AType ArrowType AscribeTerm FieldType FieldTypes TermSeq TyBinder Type OType.

Terminals
    %% Keyword tokens
    inert lambda ttop fix letrec ustring unit uunit if then else true false bool timesfloat
    ufloat leq succ pred iszero nat some let in as all

    %% Identifier and constant value tokens
    ucid lcid int_value float_value string_value

    %% Symbolic tokens
    arrow colon comma comment dot eq lcurly lparen lsquare rcurly rparen
    rsquare semi star uscore.


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
                                      B = binding({ty_var_bind, ty(top)}),
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
Command -> lcurly ucid comma lcid rcurly eq Term
                            : fun (Ctx) ->
                                      Ctx1 = add_name(Ctx, string_value('$2')),
                                      Ctx2 = add_name(Ctx1, string_value('$4')),
                                      Cmd = binding({some_bind,
                                                     info('$1'),
                                                     string_value('$2'),
                                                     string_value('$4'),
                                                     '$7'(Ctx)}),
                                      {Cmd, Ctx2}
                              end.


%% Right-hand sides of top-level bindings
Binder -> colon Type    : fun (Ctx) -> binding({var_bind, '$2'(Ctx)}) end.
Binder -> eq Term       : fun (Ctx) -> binding({tm_abb_bind, '$2'(Ctx), none}) end.


%% All type expressions
Type -> ArrowType               : '$1'.
Type -> all ucid dot Type       : fun (Ctx) ->
                                          OType = ty(top),
                                          NewCtx = add_name(Ctx, string_value('$2')),
                                          ty({all, string_value('$2'), OType, '$4'(NewCtx)})
                                  end.
Type -> all ucid OType dot Type : fun (Ctx) ->
                                          OType = '$3'(Ctx),
                                          NewCtx = add_name(Ctx, string_value('$2')),
                                          ty({all, string_value('$2'), OType, '$5'(NewCtx)})
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
AType -> ttop                       : fun (_Ctx) -> ty(top) end.
AType -> bool                       : fun (_Ctx) -> ty(bool) end.
AType -> lcurly rcurly              : fun (_Ctx) -> ty({record, []}) end.
AType -> lcurly FieldTypes rcurly   : fun ( Ctx) -> ty({record, '$2'(Ctx, 1)}) end.
AType -> ustring                    : fun (_Ctx) -> ty(string) end.
AType -> uunit                      : fun (_Ctx) -> ty(unit) end.
AType -> ufloat                     : fun (_Ctx) -> ty(float) end.
AType -> nat                        : fun (_Ctx) -> ty(nat) end.
AType -> lcurly some ucid comma Type rcurly :
        fun (Ctx) ->
            OType = ty(top),
            NewCtx = add_name(Ctx, string_value('$3')),
            ty({some, string_value('$3'), OType, '$5'(NewCtx)})
        end.
AType -> lcurly some ucid OType comma Type rcurly :
        fun (Ctx) ->
            OType = '$4'(Ctx),
            NewCtx = add_name(Ctx, string_value('$3')),
            ty({some, string_value('$3'), OType, '$6'(NewCtx)})
        end.

TyBinder -> leq Type                : fun ( Ctx) -> binding({ty_var_bind, '$2'(Ctx)}) end.
TyBinder -> eq Type                 : fun ( Ctx) -> binding({ty_abb_bind, '$2'(Ctx)}) end.


%% An "arrow type" is a sequence of atomic types separated by arrows
ArrowType -> AType arrow ArrowType  : fun (Ctx) -> ty({arr, '$1'(Ctx), '$3'(Ctx)}) end.
ArrowType -> AType                  : '$1'.

Term -> AppTerm :
        '$1'.
Term -> lambda lcid colon Type dot Term :
        fun (Ctx) ->
            NewCtx = add_name(Ctx, string_value('$2')),
            term_({abs, info('$1'), string_value('$2'), '$4'(Ctx), '$6'(NewCtx)})
        end.
Term -> lambda uscore colon Type dot Term :
        fun (Ctx) ->
            NewCtx = add_name(Ctx, "_"),
            term_({abs, info('$1'), "_", '$4'(Ctx), '$6'(NewCtx)})
        end.
Term -> if Term then Term else Term :
        fun (Ctx) ->
            term_({if_, info('$1'), '$2'(Ctx), '$4'(Ctx), '$6'(Ctx)})
        end.
Term -> let lcid eq Term in Term :
        fun (Ctx) ->
            X = string_value('$2'),
            {let_, info('$1'), X, '$4'(Ctx), '$6'(add_name(Ctx, X))}
        end.
Term -> let uscore eq Term in Term :
        fun (Ctx) ->
            X = "_",
            {let_, info('$1'), X, '$4'(Ctx), '$6'(add_name(Ctx, X))}
        end.
Term -> letrec lcid colon Type eq Term in Term :
        fun (Ctx) ->
            Info = info('$1'),
            X = string_value('$2'),
            NewCtx = add_name(Ctx, X),
            Abs = term_({abs, Info, X, '$4'(Ctx), '$6'(NewCtx)}),
            Fix = term_({fix, Info, Abs}),
            term_({let_, Info, X, Fix, '$8'(NewCtx)})
        end.
Term -> let lcurly ucid comma lcid rcurly eq Term in Term :
        fun (Ctx) ->
            TyX = string_value('$3'),
            X = string_value('$5'),
            Ctx1 = add_name(Ctx, TyX),
            Ctx2 = add_name(Ctx1, X),
            term_({unpack, info('$1'), TyX, X, '$8'(Ctx), '$10'(Ctx2)})
        end.
Term -> lambda ucid dot Term :
        fun (Ctx) ->
            OType = ty(top),
            NewCtx = add_name(Ctx, string_value('$2')),
            term_({ty_abs, info('$1'), string_value('$2'), OType, '$4'(NewCtx)})
        end.
Term -> lambda ucid OType dot Term :
        fun (Ctx) ->
            OType = '$3'(Ctx),
            NewCtx = add_name(Ctx, string_value('$2')),
            term_({ty_abs, info('$1'), string_value('$2'), OType, '$5'(NewCtx)})
        end.


AppTerm -> PathTerm                     : '$1'.
AppTerm -> AppTerm PathTerm             : fun (Ctx) ->
                                              E1 = '$1'(Ctx),
                                              E2 = '$2'(Ctx),
                                              term_({app, term_info(E1), E1, E2})
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
AppTerm -> AppTerm lsquare Type rsquare : fun (Ctx) ->
                                              T1 = '$1'(Ctx),
                                              T2 = '$3'(Ctx),
                                              term_({ty_app, term_info(T1), T1, T2})
                                          end.

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

FieldTypes -> FieldType                     : fun (Ctx, I) -> ['$1'(Ctx, I)] end.
FieldTypes -> FieldType comma FieldTypes    : fun (Ctx, I) -> ['$1'(Ctx, I) | '$3'(Ctx, I+1)] end.

FieldType -> lcid colon Type        : fun (Ctx, _) -> {string_value('$1'), '$3'(Ctx)} end.
FieldType -> Type                   : fun (Ctx, I) -> {integer_to_list(I), '$1'(Ctx)} end.

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
ATerm -> lcid                       : fun ( Ctx) ->
                                            Index = name_to_index(info('$1'), Ctx, string_value('$1')),
                                            term_({var, info('$1'), Index, context_length(Ctx)})
                                      end.
ATerm -> string_value               : fun (_Ctx) -> term_({string, info('$1'), string_value('$1')}) end.
ATerm -> unit                       : fun (_Ctx) -> term_({unit, info('$1')}) end.
ATerm -> lcurly rcurly              : fun (_Ctx) -> term_({record, info('$1'), []}) end.
ATerm -> lcurly Fields rcurly       : fun ( Ctx) -> term_({record, info('$1'), '$2'(Ctx, 1)}) end.
ATerm -> true                       : fun (_Ctx) -> term_({true, info('$1')}) end.
ATerm -> false                      : fun (_Ctx) -> term_({false, info('$1')}) end.
ATerm -> float_value                : fun (_Ctx) -> term_({float, info('$1'), float_value('$1')}) end.
ATerm -> int_value                  : fun (_Ctx) -> term_(int_value_term('$1')) end.
ATerm -> lcurly star Type comma Term rcurly as Type :
         fun (Ctx) ->
             term_({pack, info('$1'), '$3'(Ctx), '$5'(Ctx), '$8'(Ctx)})
         end.

Fields -> Field                 : fun (Ctx, I) -> ['$1'(Ctx, I)] end.
Fields -> Field comma Fields    : fun (Ctx, I) -> ['$1'(Ctx, I) | '$3'(Ctx, I+1)] end.

Field -> lcid eq Term   : fun (Ctx, _) -> {string_value('$1'), '$3'(Ctx)} end.
Field -> Term           : fun (Ctx, I) -> {integer_to_list(I), '$1'(Ctx)} end.

OType -> leq Type : '$2'.

Erlang code.

-import(fullfsub_syntax, [ty/1,
                          binding/1,
                          command/1,
                          info/1,
                          term_/1]).

-import(fullfsub_syntax, [add_name/2,
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
