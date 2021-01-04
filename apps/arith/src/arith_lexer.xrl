Definitions.

WS  = [\s\009\012]

Rules.

{WS}+ :
    skip_token.

\/\* :
    {token, {comment_start, TokenLine, TokenChars}}.

\*\/ :
    {token, {comment_end, TokenLine, TokenChars}}.

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

%% Erlance code is taken out to a separate file,
%% as Leex special syntax breaks code indendation / syntax highlighting.
-include("arith_lexer.hrl").
