Nonterminals exp stm stm_seq_aux stm_seq var_decl var_decls type fun_decl param_decls param_decl fun_decls program.
Terminals '+' '-' '*' '<=' '==' '?' ':' '(' ')' 'true' 'false' '&&' '||' ':=' ';' '::'
          integer identifier
          skip if then else while do begin end var int bool function ret.
Rootsymbol program.


Right 5 ';'.
Nonassoc 7 ':='.
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
exp -> exp '+' exp  : { exp, add, ast_line('$1'), [{lhs, '$1'}, {rhs, '$3'}] }.
exp -> exp '-' exp  : { exp, sub, ast_line('$1'), [{lhs, '$1'}, {rhs, '$3'}] }.
exp -> exp '*' exp  : { exp, mul, ast_line('$1'), [{lhs, '$1'}, {rhs, '$3'}] }.
exp -> exp '<=' exp : { exp, lt, ast_line('$1'), [{lhs, '$1'}, {rhs, '$3'}] }.
exp -> exp '==' exp : { exp, eq, ast_line('$1'), [{lhs, '$1'}, {rhs, '$3'}] }.
exp -> exp '&&' exp : { exp, 'and', ast_line('$1'), [{lhs, '$1'}, {rhs, '$3'}] }.
exp -> exp '||' exp : { exp, 'or', ast_line('$1'), [{lhs, '$1'}, {rhs, '$3'}] }.
exp -> exp '?' exp ':' exp 
                    : { exp, conditional, ast_line('$1'), [{condition, '$1'}, {'if', '$3'}, {'else', '$5'}] }.
exp -> '(' exp ')'  : '$2'.

stm -> skip          
  : { stm, skip, token_line('$1'), [] }.
stm -> identifier ':=' exp
  : { stm, assignment, token_line('$1'), [{lhs, token_value('$1')}, {rhs, '$3'}] }.
stm -> if exp then stm_seq else stm_seq
  : { stm, 'if', token_line('$1'), [{condition, '$2'}, {'then', '$4'}, {'else', '$6'}] }.
stm -> while exp do stm_seq
  : { stm, while, token_line('$1'), [{condition, '$2'}, {'body', '$4'}] }.
stm -> begin var_decls ';' stm_seq end
  : { stm, block, token_line('$1'), [{decls, '$2'}, {body, '$4'}]}.


program -> fun_decls stm_seq
  : {program, program, program_line('$1', '$2'), [{functions, '$1'}, {main_stm, '$2'}]}.

fun_decls -> '$empty'             : [].
fun_decls -> fun_decl fun_decls   : ['$1' | '$2'].

fun_decl -> function identifier '(' param_decls ')' ret '(' param_decls ')' stm_seq end
  : {declaration, fun_decl, token_line('$1'), [{function_name, token_value('$2')}, {params, '$4'}, {returns, '$8'}, {body, '$10'}]}.

stm_seq -> stm_seq_aux      : '$1'.
stm_seq -> stm_seq_aux ';'  : '$1'.

stm_seq_aux -> stm                 : ['$1'].
stm_seq_aux -> stm ';' stm_seq_aux : ['$1' | '$3'].

var_decls -> '$empty'                : [].
var_decls -> var_decl ';' var_decls  : ['$1' | '$3'].

var_decl -> var identifier ':=' exp           
  : {declaration, var_decl, token_line('$1'), [{lhs, token_value('$2')}, {rhs, '$4'}]}.
var_decl -> var identifier '::' type ':=' exp
  : {declaration, var_decl, token_line('$1'), [{lhs, token_value('$2')}, {rhs, '$6'}, {type, '$4'}]}.


param_decls -> '$empty'                    : [].
param_decls -> param_decl ';' param_decls  : ['$1' | '$3'].

param_decl -> identifier 
  : [{declaration, param_decl, token_line('$1'), [{variable, token_value('$1')}]}].
param_decl -> identifier '::' type 
  : [{declaration, param_decl, token_line('$1'), [{variable, token_value('$1')}, {type, '$3'}]}].


type -> int   : {type, int, token_line('$1'), []}.
type -> bool  : {type, bool, token_line('$1'), []}.

Erlang code.

token_value({_, Val}) -> Val;
token_value({_, _, Val}) -> Val.
token_line({_, Line}) -> Line;
token_line({_, Line, _}) -> Line.



program_line([], [Stm | _]) -> ast_line(Stm);
program_line([D|_], _Stms) -> ast_line(D).

ast_line({_, _, Line, _}) -> Line.
