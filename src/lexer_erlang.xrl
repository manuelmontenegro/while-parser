Definitions.

Letter = [a-zA-Z]
LetterOrUnderscore = {Letter}|_
Digit = [0-9]
Whitespace = [\s\t\n\r]
Integer = {Digit}+
Identifier = ({LetterOrUnderscore})({LetterOrUnderscore}|{Digit})*

Rules.

{Whitespace}  : skip_token.
\(            : {token, {'(', TokenLine}}.
\)            : {token, {')', TokenLine}}.
::            : {token, {'::', TokenLine}}.
\+            : {token, {'+', TokenLine}}.
\-            : {token, {'-', TokenLine}}.
\*            : {token, {'*', TokenLine}}.
\<\=          : {token, {'<=', TokenLine}}.
\?            : {token, {'?', TokenLine}}.
:             : {token, {':', TokenLine}}.
:=            : {token, {':=', TokenLine}}.
==            : {token, {'==', TokenLine}}.
\;            : {token, {';', TokenLine}}.
\,            : {token, {',', TokenLine}}.
\&\&          : {token, {'&&', TokenLine}}.
\|\|          : {token, {'||', TokenLine}}.
skip          : {token, {skip, TokenLine}}.
function      : {token, {function, TokenLine}}.
ret           : {token, {ret, TokenLine}}.
end           : {token, {'end', TokenLine}}.
true          : {token, {'true', TokenLine}}.
false         : {token, {'false', TokenLine}}.
in            : {token, {in, TokenLine}}.
if            : {token, {'if', TokenLine}}.
then          : {token, {'then', TokenLine}}.
else          : {token, {'else', TokenLine}}.
while         : {token, {'while', TokenLine}}.
do            : {token, {'do', TokenLine}}.
begin         : {token, {'begin', TokenLine}}.
var           : {token, {'var', TokenLine}}.
int           : {token, {'int', TokenLine}}.
bool          : {token, {'bool', TokenLine}}.
{Identifier}  : {token, {identifier, TokenLine, list_to_binary(TokenChars)}}.
{Integer}{LetterOrUnderscore} 
              : {error, ["Invalid token \"", TokenChars, "\""]}.
{Integer}     : {token, {integer, TokenLine, list_to_integer(TokenChars)}}.

Erlang code.
