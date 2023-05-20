%{
//-- don't change *any* of these: if you do, you'll break the compiler.
#include <algorithm>
#include <memory>
#include <cstring>
#include <cdk/compiler.h>
#include <cdk/types/types.h>
#include ".auto/all_nodes.h"
#define LINE                         compiler->scanner()->lineno()
#define yylex()                      compiler->scanner()->scan()
#define yyerror(compiler, s)         compiler->scanner()->error(s)
//-- don't change *any* of these --- END!
%}

%parse-param {std::shared_ptr<cdk::compiler> compiler}

%union {
  //--- don't change *any* of these: if you do, you'll break the compiler.
  YYSTYPE() : type(cdk::primitive_type::create(0, cdk::TYPE_VOID)) {}
  ~YYSTYPE() {}
  YYSTYPE(const YYSTYPE &other) { *this = other; }
  YYSTYPE& operator=(const YYSTYPE &other) { type = other.type; return *this; }

  std::shared_ptr<cdk::basic_type> type;        /* expression type */
  //-- don't change *any* of these --- END!

  int                   i;	/* integer value */
  std::string          *s;	/* symbol name or string literal */
  double                d;	/* double value */
  cdk::basic_node      *node;	/* node pointer */
  cdk::sequence_node   *sequence;
  cdk::expression_node *expression; /* expression nodes */
  cdk::lvalue_node     *lvalue;
};

%token <i> tINTEGER
%token <s> tIDENTIFIER tSTRING
%token <d> tDOUBLE
%token tWHILE tIF tELIF tELSE tPRINT tPRINTLN tINPUT tBEGIN tEND tSIZEOF
%token tPUBLIC tFORWARD tFOREIGN tPRIVATE /* qualifier tokens */
%token tNULLPTR
%token tAUTO_TYPE tINT_TYPE tREAL_TYPE tSTR_TYPE tVOID_TYPE /* type tokens */
%token tSTOP tNEXT tRETURN

%nonassoc tIFX
%nonassoc tELSE

%right '='
%left tGE tLE tEQ tNE '>' '<'
%left '+' '-'
%left '*' '/' '%'
%nonassoc tUNARY

%type <node> stmt program
%type <sequence> list
%type <sequence> exprs
%type <expression> expr
%type <lvalue> lval

%{
//-- The rules below will be included in yyparse, the main parsing function.
%}
%%

program	: tBEGIN list tEND { }
	     ;

list : stmt	     { $$ = new cdk::sequence_node(LINE, $1); }
	| list stmt { $$ = new cdk::sequence_node(LINE, $2, $1); }
	;

stmt : expr ';'                          { $$ = new mml::evaluation_node(LINE, $1); }
 	| exprs tPRINT                      { $$ = new mml::write_node(LINE, $1, false); }
 	| exprs tPRINTLN                    { $$ = new mml::write_node(LINE, $1, true); }
     | tINPUT                            { $$ = new mml::input_node(LINE); }
     | tWHILE '(' expr ')' stmt          { $$ = new mml::while_node(LINE, $3, $5); }
     | tIF '(' expr ')' stmt %prec tIFX  { $$ = new mml::if_node(LINE, $3, $5); }
     | tIF '(' expr ')' stmt tELSE stmt  { $$ = new mml::if_else_node(LINE, $3, $5, $7); }
     | '{' list '}'                      { $$ = $2; }
     | tSTOP ';'                         { $$ = new mml::stop_node(LINE, 1); }
     | tSTOP tINTEGER ';'                { $$ = new mml::stop_node(LINE, $2); }
     | tNEXT ';'                         { $$ = new mml::next_node(LINE, 1); }
     | tNEXT tINTEGER ';'                { $$ = new mml::next_node(LINE, $2); }
     | tRETURN expr ';'                  { $$ = new mml::return_node(LINE, $2); }
     | block                             { $$ = $1; }  
     ;

stmts : stmt        { $$ = new cdk::sequence_node(LINE, $1); }
      | stmts stmt  { $$ = new cdk::sequence_node(LINE, $2, $1); }
      ;


opt_statements : /* empty */  { $$ = sequence_node(LINE); }
               | stmts      { $$ = $1; }
               ;                 

elif_stmt : /* empty */ { $$ = nullptr; }
          | tELIF '(' expr ')' stmt elif_stmt { $$ = new mml::if_node(LINE, $3, $5); }
          ;
                 
exprs : expr              { $$ = new cdk::sequence_node(LINE, $1); }
      | exprs ',' expr    { $$ = new cdk::sequence_node(LINE, $3, $1); }
      ;

expr : tINTEGER                    { $$ = new cdk::integer_node(LINE, $1); }
     | tSTRING                     { $$ = new cdk::string_node(LINE, $1); }
     | '-' expr %prec tUNARY       { $$ = new cdk::neg_node(LINE, $2); }
     | expr '+' expr	          { $$ = new cdk::add_node(LINE, $1, $3); }
     | expr '-' expr	          { $$ = new cdk::sub_node(LINE, $1, $3); }
     | expr '*' expr	          { $$ = new cdk::mul_node(LINE, $1, $3); }
     | expr '/' expr	          { $$ = new cdk::div_node(LINE, $1, $3); }
     | expr '%' expr	          { $$ = new cdk::mod_node(LINE, $1, $3); }
     | expr '<' expr	          { $$ = new cdk::lt_node(LINE, $1, $3); }
     | expr '>' expr	          { $$ = new cdk::gt_node(LINE, $1, $3); }
     | expr tGE expr	          { $$ = new cdk::ge_node(LINE, $1, $3); }
     | expr tLE expr               { $$ = new cdk::le_node(LINE, $1, $3); }
     | expr tNE expr	          { $$ = new cdk::ne_node(LINE, $1, $3); }
     | expr tEQ expr	          { $$ = new cdk::eq_node(LINE, $1, $3); }
     | '(' expr ')'                { $$ = $2; }
     | lval                        { $$ = new cdk::rvalue_node(LINE, $1); }
     | lval '=' expr               { $$ = new cdk::assignment_node(LINE, $1, $3); }
     | tNULLPTR                    { $$ = new mml::nullptr_node(LINE); }
     | func_def                    { $$ = $1; }
     | tSIZEOF '(' expr ')'        { $$ = new mml::sizeof_node(LINE, $3); }
     | '+' tINT_TYPE %prec tUNARY   { $$ = new mml::identity_node(LINE, $2); }
     | '+' tREAL_TYPE    %prec tUNARY   { $$ = new mml::identity_node(LINE, $2); }
     | '-' tINT_TYPE %prec tUNARY   { $$ = new cdk::neg_node(LINE, $2); }
     | '-' tREAL_TYPE    %prec tUNARY   { $$ = new cdk::neg_node(LINE, $2); }
     ;

expr_assig : '=' expr { $$ = $2; }
           ;

opt_expr_assig : /* empty */  { $$ = nullptr; }
               | expr_assig   { $$ = $1; }
               ;


type  : tINT_TYPE          { $$ = cdk::primitive_type::create(4, cdk::TYPE_INT); }
      | tREAL_TYPE         { $$ = cdk::primitive_type::create(8, cdk::TYPE_DOUBLE); }
      | tSTR_TYPE          { $$ = cdk::primitive_type::create(4, cdk::TYPE_STRING); }
      | '[' tVOID_TYPE ']' { $$ = cdk::pointer_type::create(4, cdk::primitive_type::create(4, cdk::TYPE_VOID)); }
      | '[' type ']'  
      /* Checks for nested void pointers and ignores them (eg. `[[[void]]]` == `[void]`) */
      { 
          /* Case where `type` == `[...[[void]]...]`. Follows the previous rule (i.e. creates a `[void]` pointer). */
          if ($2->name() == cdk::TYPE_POINTER && cdk::reference_type::cast($2)->referenced()->name() == cdk::TYPE_VOID) {
               $$ = cdk::pointer_type::create(4, cdk::primitive_type::create(4, cdk::TYPE_VOID));
          }
          /* Case where `type` == `[...[[any_type]]...]` */
          else {
               $$ = cdk::reference_type::create(4, $2);
          }
      } 
      | func_type          { $$ = $1; }
      ;
      
      
types : type           { $$ = new std::vector<std::shared_ptr<cdk::basic_type>>(); $$->push_back(*$1); delete $1; }
      | types ',' type { $$ = $1; $$->push_back(*$3); delete $3; }
      ;


func_type : type '<' '>'              { $$ = cdk::functional_type::create($1); }
          | type '<' types '>'        { $$ = cdk::functional_type::create($3, $1); }
          | tVOID_TYPE '<' '>'        { $$ = cdk::functional_type::create(cdk::primitive_type::create(4, cdk::TYPE_VOID)); }
          | tVOID_TYPE '<' types '>'  { $$ = cdk::functional_type::create($3, cdk::primitive_type::create(4, cdk::TYPE_VOID)); }
          ;

func_def  : '(' opt_vars ')' '-' '>' type block { $$ = new mml::function_def_node(LINE, $2, $6, $7); }
          ;
          
opt_vars : /* empty */ { $$ = new cdk::sequence_node(LINE); }
         | vars        { $$ = $1; }
         ;
         
vars : var          { $$ = new cdk::sequence_node(LINE, $1); }
     | vars ',' var { $$ = new cdk::sequence_node(LINE, $3, $1); }
     ;


declaration :          type tIDENTIFIER opt_expr_assig ';'   { $$ = new mml::declaration_node(LINE, tPRIVATE, $2, $3, $1); }
            | tPUBLIC  type tIDENTIFIER opt_expr_assig ';'   { $$ = new mml::declaration_node(LINE, tPUBLIC , $3, $4, $2); }
            | tFORWARD type tIDENTIFIER opt_expr_assig ';'   { $$ = new mml::declaration_node(LINE, tPUBLIC , $3, $4, $2); }
            | tFOREIGN type tIDENTIFIER opt_expr_assig ';'   { $$ = new mml::declaration_node(LINE, tPUBLIC , $3, $4, $2); }
            |          tAUTO_TYPE tIDENTIFIER expr_assig ';' { $$ = new mml::declaration_node(LINE, tPRIVATE, $2, $3, nullptr); }
            | tPUBLIC  tAUTO_TYPE tIDENTIFIER expr_assig ';' { $$ = new mml::declaration_node(LINE, tPUBLIC , $3, $4, nullptr); }
         /* | tFORWARD tAUTO_TYPE tIDENTIFIER expr_assig ';' { $$ = new mml::declaration_node(LINE, tPUBLIC , $3, $4, nullptr); } */
            | tFOREIGN tAUTO_TYPE tIDENTIFIER expr_assig ';' { $$ = new mml::declaration_node(LINE, tPUBLIC , $3, $4, nullptr); }
         /* |          tIDENTIFIER expr_assig ';'            { $$ = new mml::declaration_node(LINE, tPRIVATE, $2, $3, $1); } */
            | tPUBLIC  tIDENTIFIER expr_assig ';'            { $$ = new mml::declaration_node(LINE, tPUBLIC , $3, $4, $2); }
         /* | tFORWARD tIDENTIFIER expr_assig ';'            { $$ = new mml::declaration_node(LINE, tPUBLIC , $3, $4, $2); } */
         /* | tFOREIGN tIDENTIFIER expr_assig ';'            { $$ = new mml::declaration_node(LINE, tPUBLIC , $3, $4, $2); } */
            ;    
     
declarations : declaration               { $$ = new cdk::sequence_node(LINE, $1); }
             | declarations declaration  { $$ = new cdk::sequence_node(LINE, $2, $1); }
             ;

opt_declarations : /* empty */   { $$ = sequence_node(LINE); }
                 | declarations  { $$ = $1; }
                 ;

block : '{' opt_declarations opt_statements '}'  { $$ = new mml::block_node(LINE, $2, $3); }
      ;

lval : tIDENTIFIER             { $$ = new cdk::variable_node(LINE, $1); }
     ;
%%
