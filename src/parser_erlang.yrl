Nonterminals exp.
Terminals '+' '-' '*' '<=' '==' '?' ':' '(' ')' 'true' 'false' '&&' '||' integer identifier.
Rootsymbol exp.


Nonassoc 10 '?'.
Nonassoc 10 ':'.
Right 30 '||'.
Right 40 '&&'.
Nonassoc 50 '<='.
Nonassoc 50 '=='.
Left 100 '+'.
Left 100 '-'.
Left 200 '*'.

exp -> integer      : { exp, literal, token_line('$1'), [{number, token_value('$1')}] }.
exp -> 'true'       : { exp, literal, token_line('$1'), [{boolean, true}]}.
exp -> 'false'       : { exp, literal, token_line('$1'), [{boolean, true}]}.
exp -> identifier   : { exp, variable, token_line('$1'), [{name, token_value('$1')}] }.
exp -> '-' integer  : { exp, literal, token_line('$1'), [{number, -token_value('$2')}]}.
exp -> exp '+' exp  : { exp, add, exp_line('$1'), [{lhs, '$1'}, {rhs, '$3'}] }.
exp -> exp '-' exp  : { exp, sub, exp_line('$1'), [{lhs, '$1'}, {rhs, '$3'}] }.
exp -> exp '*' exp  : { exp, mul, exp_line('$1'), [{lhs, '$1'}, {rhs, '$3'}] }.
exp -> exp '<=' exp : { exp, lt, exp_line('$1'), [{lhs, '$1'}, {rhs, '$3'}] }.
exp -> exp '==' exp : { exp, eq, exp_line('$1'), [{lhs, '$1'}, {rhs, '$3'}] }.
exp -> exp '&&' exp : { exp, 'and', exp_line('$1'), [{lhs, '$1'}, {rhs, '$3'}] }.
exp -> exp '||' exp : { exp, 'or', exp_line('$1'), [{lhs, '$1'}, {rhs, '$3'}] }.
exp -> exp '?' exp ':' exp 
                    : { exp, conditional, exp_line('$1'), [{condition, '$1'}, {'if', '$3'}, {'else', '$5'}] }.
exp -> '(' exp ')'  : '$2'.


Erlang code.

token_value({_, Val}) -> Val;
token_value({_, _, Val}) -> Val.
token_line({_, Line}) -> Line;
token_line({_, Line, _}) -> Line.



exp_line({exp, _, Line, _}) -> Line.
