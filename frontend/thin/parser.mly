/* Parser tokens definitions */

%{
    let anonym_structs = ref []
    let anonym_unions = ref []
(*     let anonym_functions = ref [] *)
    
    [@@@coverage exclude_file]
    open Ast
    open Past
%}

/* Token definitions */

%token <int> INT
%token <char> CHAR GENERIC
%token <string> ID STRING MODNAME
%token LPAREN RPAREN LBRACE RBRACE LANGLE RANGLE DLANGLE DRANGLE LARROW RARROW FATLARROW FATRARROW COMMA DOT COLON SEMICOLON EQUAL TAKE
%token RETURN
%token PLUS MINUS MULT DIV MOD POWER ROOT AND OR DAND DOR EXCLAMATION_MARK TILDE WILDCARD
%token LET CONST MUT FUNCTION IN REF ASYNC STRUCT UNION ENUM INCLUDE MATCH
%token T_I8 T_I16 T_I32 T_I64 T_I128 T_U8 T_U16 T_U32 T_U64 T_U128 T_BOOL T_EMPTY
%token BORROWED TRUE FALSE IF ELSE FOR WHILE EOF

/* 
%left  we reduce
%right we shift
%nonassoc raise a syntax error 
We list the operators in order of precedence - from low to high.
e.g. * has higher precedence than +  so 1 + 2 * 3  = 1 + (2 * 3)
*/

%right EQUAL LBRACE RBRACE LPAREN RPAREN 
%left LANGLE RANGLE DLANGLE DRANGLE LARROW RARROW FATLARROW FATRARROW
%left PLUS MINUS
%left MULT DIV MOD
%left POWER ROOT
%left DOR DAND AND OR 
%nonassoc EXCLAMATION_MARK TILDE RETURN

%start program 

/* Types for the result of productions */

%type <Past.program> program

/* struct defn types */
%type <struct_defn> struct_defn
%type <generic_type> generic_type
%type <borrowed_ref> borrowed_ref
%type <modifier> modifier
%type <field_defn> field_defn
%type <param list> params
%type <param> param

%type <method_defn> method_defn
%type <function_defn> function_defn

%type <type_expr> parameterised_type
%type <type_expr> type_expr
%type <type_expr> maybe_type

%type <block_expr> block_expr
%type <expr list> args
%type <identifier> identifier
%type <expr> expr
%type <async_expr> async_expr

%type <un_op> un_op
%type <bin_op> bin_op

%% /* Start grammar productions */

program: 
    | struct_defns=list(struct_defn);
      union_defns=list(union_defn);
      function_defns=list(function_defn);
      EOF 
      {Prog(
        struct_defns @ !anonym_structs, 
        union_defns @ !anonym_unions, 
        function_defns)
      }

/* Imports / Preprocessing */
// TODO

/* Productions related to struct/union/enums definitions */

struct_defn:
    | STRUCT; name=option(ID); maybe_generic=option(generic_type); EQUAL LBRACE; field_defns=list(field_defn); method_defns=list(method_defn); RBRACE 
        {TStruct(Struct_name.of_string (name_or_anonym name), maybe_generic, field_defns, method_defns)}
    | STRUCT; name=option(ID); maybe_generic=option(generic_type); EQUAL field_defns=list(field_defn);
        {TStruct(Struct_name.of_string (name_or_anonym name), maybe_generic, field_defns, [])}
    | STRUCT; name=option(ID); maybe_generic=option(generic_type); SEMICOLON 
        {TStruct(Struct_name.of_string (name_or_anonym name), maybe_generic, [], [])}

union_defn:
    | UNION; name=option(ID); maybe_generic=option(generic_type); EQUAL; LBRACE; field_defns=list(field_defn); method_defns=list(method_defn); RBRACE 
        {TUnion(Union_name.of_string (name_or_anonym name), maybe_generic, field_defns, method_defns)}
    | UNION; name=option(ID); maybe_generic=option(generic_type); EQUAL; field_defns=list(field_defn) 
        {TUnion(Union_name.of_string (name_or_anonym name), maybe_generic, field_defns, [])}
    | ENUM; name=option(ID); maybe_generic=option(generic_type); EQUAL; option(OR); variant_defns=separated_nonempty_list(OR, variant_defn) 
        {TUnion(Union_name.of_string (name_or_anonym name), maybe_generic, variant_defns, [])}
    | UNION; name=option(ID); maybe_generic=option(generic_type); SEMICOLON 
        {TUnion(Union_name.of_string (name_or_anonym name), maybe_generic, [], [])}
    | ENUM; name=option(ID); maybe_generic=option(generic_type); SEMICOLON 
        {TUnion(Union_name.of_string (name_or_anonym name), maybe_generic, [], [])}

generic_type:
    | LANGLE generics=separated_nonempty_list(COMMA, GENERIC) RANGLE { Generic(generics) }

borrowed_ref:
    | BORROWED {Borrowed}

maybe_type:
    | COLON; param_type=type_expr {param_type} 

/* Field definitions */

modifier:
    | CONST {MConst}
    | MUT {Mmut}

field_defn:
    | maybeModifier=option(modifier); field_name=ID; maybe_type=option(maybe_type); SEMICOLON {TField(maybeModifier, maybe_type, Field_name.of_string field_name)}

variant_defn:
    | field_name=ID; maybe_type=option(maybe_type); {TField(None, maybe_type, Field_name.of_string field_name)}

/* Method and function definitions */

param:
    | maybeBorrowed=option(borrowed_ref); maybeModifier=option(modifier) param_name=ID; maybe_type=option(maybe_type) {TParam(maybe_type, Var_name.of_string param_name, maybeBorrowed, maybeModifier)}

params:
    | params=separated_list(COMMA,param) {params}

function_defn: 
    | FUNCTION; maybeBorrowed=option(borrowed_ref); return_type=type_expr; RETURN; function_name=ID; function_params=params; EQUAL; body=block_expr {TFunction(Function_name.of_string function_name, maybeBorrowed, return_type, function_params, body)}

method_defn: 
    | maybeBorrowed=option(borrowed_ref); return_type=type_expr; LANGLE; method_name=ID; method_params=params; EQUAL; body=block_expr {TMethod(Method_name.of_string method_name, maybeBorrowed, return_type, method_params, body)}

/* Types */

parameterised_type:
    | LANGLE type_param=type_expr RANGLE {type_param}

type_expr: 
    | TILDE { TEDiverge }
    | WILDCARD { TEWild }
    | T_U8 {TEu8} | T_U16 {TEu16} | T_U32 {TEu32} | T_U64 {TEu64} | T_U128 {TEu128}
    | T_I8 {TEi8} | T_I16 {TEi16} | T_I32 {TEi32} | T_I64 {TEi64} | T_I128 {TEi128}
    | T_BOOL {TEBool}
    | T_EMPTY {TEEmpty}
    | g=GENERIC { TEGeneric(g) }
    | s=struct_type_expr {s}
    | u=union_type_expr {u}
    | t=tuple_type_expr {t}

tuple_type_expr:
    | LPAREN; types=separated_list(COMMA, type_expr); RPAREN { TEtuple(types) }

struct_type_expr:
    | struct_name=ID maybe_param_type=option(parameterised_type) {TEstruct(Struct_name.of_string struct_name, maybe_param_type)}
    | struct_defn=struct_defn {anonym_structs := !anonym_structs @ [struct_defn]; type_expr_of_struct struct_defn}

union_type_expr:
    | union_name=ID maybe_param_type=option(parameterised_type) {TEunion(Union_name.of_string union_name, maybe_param_type)}
    | union_defn=union_defn {anonym_unions := !anonym_unions @ [union_defn]; type_expr_of_union union_defn}

/* Method / function arguments */
args:
    | args=separated_list(COMMA, expr) {args}

identifier:
    | variable=ID { Variable(Var_name.of_string variable) }
    | obj=ID DOT field=ID { StructField(Var_name.of_string obj, Field_name.of_string field) }
    | DOT field=ID { SelfField(Field_name.of_string field) }

/* Expressions */
block_expr:
    | LBRACE; exprs=separated_list(SEMICOLON, option(expr)); RBRACE {Block($startpos, exprs)}
    | expr=option(expr); SEMICOLON {Block($startpos, [expr])}

%inline for_expr:
    | init_expr=expr; SEMICOLON; cond_expr=expr; SEMICOLON; step_expr=expr; loop_expr=block_expr {For($startpos, init_expr, cond_expr, step_expr, loop_expr)} 

%inline if_expr:
    | cond_expr=expr; then_expr=block_expr; ELSE; else_expr=block_expr { If($startpos, cond_expr, then_expr, Some else_expr) }
    | cond_expr=expr; then_expr=block_expr; { If($startpos, cond_expr, then_expr, None) }

struct_expr:    
    | LBRACE vals=separated_nonempty_list(COMMA, expr) RBRACE maybe_struct_t=option(ID) 
    {  
        let struct_t = (match maybe_struct_t with
            | None -> None
            | Some st -> Some (Struct_name.of_string st)
        ) in 
        Structure($startpos, struct_t, Some vals) 
    }
    | LBRACE; RBRACE; maybe_struct_t=option(ID) 
    { 
        let struct_t = (match maybe_struct_t with
            | None -> None
            | Some st -> Some (Struct_name.of_string st)
        ) in 
        Structure($startpos, struct_t, None) 
    }

tuple_expr:
    | LPAREN exprs=separated_nonempty_list(COMMA, expr) RPAREN
        { Tuple($startpos, exprs) }

expr:
    /* Classic op expressions */
    | LPAREN e=expr RPAREN {e}
    | i=INT { Integer($startpos, i) }
    | c=CHAR { Char($startpos, c) }
    | TRUE { Boolean($startpos, true)}
    | FALSE { Boolean($startpos, false) }
    | tuple=tuple_expr { tuple }
    | structure=struct_expr { structure }
    | id=identifier { Identifier($startpos, id) }
    /* Unary / Binary / Pipe / Additional ops */
    | op=un_op e=expr { UnOp($startpos, op, e) }
    | e1=expr op=bin_op e2=expr { BinOp($startpos, op, e1, e2) }
    | e1=expr op=pipe_op e2=expr { PipeOp($startpos, op, e1, e2) }
    | OR e=expr OR { UnOp($startpos, UnOpAbs, e) } 
    | DOR e=expr DOR { UnOp($startpos, UnOpNorm, e) } 
    /* Creating / reassigning / deallocating references */
    | LET; maybeModifier=option(modifier); var_name=ID; maybe_type=option(maybe_type); EQUAL; bound_expr=expr {Let($startpos, maybe_type, maybeModifier, Var_name.of_string var_name, bound_expr)} 
    | RANGLE; RANGLE; ids=separated_list(COMMA, identifier); LANGLE; LANGLE {Consume($startpos, ids)} 
    /* Function / Method Application */
    | obj=ID; DOT; method_name=ID; pipe_op; method_args=args {MethodApp($startpos, Var_name.of_string obj, Method_name.of_string method_name, method_args)}
    | fn=ID; pipe_op; fn_args=args { FunctionApp($startpos, Function_name.of_string fn, fn_args) } 
    /* Control flow */
    | IF; if_expr=if_expr {if_expr}
    | WHILE cond_expr=expr; loop_expr=block_expr {While($startpos, cond_expr, loop_expr)}
    | FOR; fexpr=for_expr {fexpr}
    | MATCH; ident_expr=expr; IN; arms=nonempty_list(match_arm) {Match($startpos, ident_expr, arms)} 
    /* Async expression */
    | ASYNC; LBRACE; forked_async_exprs=list(async_expr); curr_thread_expr=separated_list(SEMICOLON, option(expr)); RBRACE {FinishAsync($startpos, forked_async_exprs, Block($startpos(curr_thread_expr), curr_thread_expr))}

match_arm:
    | OR; larm=type_expr; RARROW; rarm=expr %prec OR { (larm, Some rarm) }
    | OR; larm=type_expr { (larm, None) }

async_expr:
    | exprs=block_expr {AsyncExpr exprs}

/* Operator expressions */

%inline un_op:
    | TILDE { UnOpNot }
    | EXCLAMATION_MARK { UnOpLNot }
    | MINUS { UnOpNeg }
    | RETURN { UnOpRet }

%inline bin_op:
    | PLUS { BinOpPlus }
    | MINUS { BinOpMinus }
    | MULT { BinOpMult }
    | DIV { BinOpIntDiv } 
    | MOD { BinOpRem }
    | POWER { BinOpPower }
    | ROOT { BinOpRoot }

    | LANGLE { BinOpLessThan }
    | LANGLE EQUAL { BinOpLessThanEq }
    | RANGLE { BinOpGreaterThan }
    | RANGLE EQUAL { BinOpGreaterThanEq }
    | AND { BinOpAnd }
    | OR { BinOpOr }
    | DOR { BinOpLOr }
    | DAND { BinOpLAnd }
    | EQUAL EQUAL { BinOpEq }
    | EXCLAMATION_MARK EQUAL { BinOpNotEq }

%inline pipe_op:
    | DLANGLE { PipeOpLeft }
    | FATLARROW { PipeOpBindLeft }
    | DRANGLE { PipeOpRight }
    | FATRARROW { PipeOpBindRight }
