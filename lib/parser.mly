%{
    open Ast
%}

%token <string> STRING
%token <string> VAR
%token <int> NUMBER

%token COMMA
%token PLUS
%token MINUS
%token MULT
%token DIV
%token LT
%token LTE
%token GT
%token GTE
%token DIFF
%token FROWN
%token EQ
%token LPAR
%token RPAR

%token PRINT
%token IF
%token THEN
%token GOTO
%token INPUT
%token LET
%token GOSUB
%token RETURN
%token CLEAR
%token LIST
%token RUN
%token END
%token NEWLINE
%token EOF

(* Priority of operators *)

%left MULT DIV
%left PLUS MINUS

(* *)

%start<Ast.line list> line_list

%%

line_list:
  l = line NEWLINE ll = line_list {l::ll}
| NEWLINE ll = line_list {ll}
| l = line EOF {[l]}
| EOF {[]}

line:
  line_number = NUMBER stat = statement {NumberedStatement(line_number, stat)}
| stat = statement {Statement(stat)}

statement:
  PRINT el = expr_list {Print(el)}
| IF ea = expr r = relop eb = expr THEN s = statement {If(ea, r, eb, s)}
| GOTO e = expr {Goto(e)}
| INPUT vl = var_list {Input(vl)}
| LET v = VAR EQ e = expr {Let(v, e)}
| GOSUB e = expr {Gosub(e)}
| RETURN {Return}
| CLEAR {Clear}
| LIST {List}
| RUN {Run}
| END {End}

expr_list:
  e = expr COMMA el = expr_list {e::el}
| e = expr {[e]}

var_list:
  v = VAR COMMA vl = var_list {v::vl}
| v = VAR {[v]}

expr:
        t = expr PLUS e = expr {Plus(t, e)}
|       t = expr MINUS e = expr {Minus(t, e)}
| PLUS  t = term {t}
| MINUS t = term {UnaryMinus(t)}
|       t = term {t}

term:
  f = term MULT t = term {Mult(f, t)}
| f = term DIV t = term {Div(f, t)}
| f = factor {f}

factor:
  v = VAR {Var(v)}
| n = NUMBER {Value(Number(n))}
| s = STRING {Value(String(s))}
| LPAR e = expr RPAR {e}

relop:
  LT {Lt}
| GT {Gt}
| LTE {Lte}
| GTE {Gte}
| EQ {Eq}
| DIFF {Diff}
| FROWN {Frown}
