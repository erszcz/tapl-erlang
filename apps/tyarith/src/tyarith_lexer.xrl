Definitions.

WS = [\s\011\012]
LCOMMENT = \/\*
RCOMMENT = \*\/

Rules.

{WS}+ :
    skip_token.

{RCOMMENT} :
    {error, "unmatched end of comment"}.

{LCOMMENT}.*{RCOMMENT} :
    {token, {comment, TokenLine, TokenChars}}.

[0-9]+ :
    {token, {int_value, TokenLine, TokenChars}}.

[0-9]+\.[0-9]+ :
    {token, {float_value, TokenLine, TokenChars}}.

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
-include("tyarith_lexer.hrl").
