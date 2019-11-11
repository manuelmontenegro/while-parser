% Copyright 2019 Manuel Montenegro
%
% Permission is hereby granted, free of charge, to any person obtaining a copy of this software
% and associated documentation files (the "Software"), to deal in the Software without restriction,
% including without limitation the rights to use, copy, modify, merge, publish, distribute,
% sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is
% furnished to do so, subject to the following conditions:
%
% The above copyright notice and this permission notice shall be included in all copies or substantial
% portions of the Software.
%
% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT
% NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
% NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES
% OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
% CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

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
\[            : {token, {'[', TokenLine}}.
\]            : {token, {']', TokenLine}}.
\|            : {token, {'|', TokenLine}}.
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
\.            : {token, {'.', TokenLine}}.
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
ifnil         : {token, {'ifnil', TokenLine}}.
then          : {token, {'then', TokenLine}}.
else          : {token, {'else', TokenLine}}.
while         : {token, {'while', TokenLine}}.
do            : {token, {'do', TokenLine}}.
begin         : {token, {'begin', TokenLine}}.
var           : {token, {'var', TokenLine}}.
int           : {token, {'int', TokenLine}}.
bool          : {token, {'bool', TokenLine}}.
hd            : {token, {'hd', TokenLine}}.
tl            : {token, {'tl', TokenLine}}.
nil           : {token, {'nil', TokenLine}}.
{Identifier}  : {token, {identifier, TokenLine, list_to_binary(TokenChars)}}.
{Integer}{LetterOrUnderscore} 
              : {error, ["Invalid token \"", TokenChars, "\""]}.
{Integer}     : {token, {integer, TokenLine, list_to_integer(TokenChars)}}.

Erlang code.
