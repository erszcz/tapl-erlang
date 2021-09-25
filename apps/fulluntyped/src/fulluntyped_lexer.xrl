Definitions.

WS = [\s\011]
NL = [\012]
LCOMMENT = \/\*
RCOMMENT = \*\/

Rules.

{RCOMMENT} :
    {error, "unmatched end of comment"}.

{LCOMMENT}[^(\*\/)]+{RCOMMENT} :
    {token, {comment, span(TokenLine, TokenChars), TokenChars}}.

{WS}+ :
    update_column(TokenChars),
    skip_token.

{NL} :
    reset_column(),
    skip_token.

[0-9]+ :
    {token, {int_value, span(TokenLine, TokenChars), TokenChars}}.

[0-9]+\.[0-9]+ :
    {token, {float_value, span(TokenLine, TokenChars), TokenChars}}.

[A-Za-z_][A-Za-z_0-9']* :
    {token, create_id(TokenLine, TokenChars)}.

[-+] :
    {token, create_id(TokenLine, TokenChars)}.

[-~%\\+&\|:@`$]+ :
    {token, create_id(TokenLine, TokenChars)}.

[*#/!\?^(){}\[\]<>\.;_,=\'] :
    {token, create_id(TokenLine, TokenChars)}.

Erlang code.

%% Erlang code is taken out to a separate file,
%% as Leex special syntax breaks code indendation / syntax highlighting.
-include("fulluntyped_lexer.hrl").
