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

  int                                            i;	/* integer value */
  std::string                                   *s;	/* symbol name or string literal */
  double                                         d;	/* double value */
  cdk::basic_node                               *node;	/* node pointer */
  cdk::sequence_node                            *sequence;
  cdk::expression_node                          *expression; /* expression nodes */
  mml::declaration_node                         *declaration;
  mml::block_node                               *block;
  cdk::lvalue_node                              *lvalue;
  mml::function_def_node                        *func_def;
  std::shared_ptr<cdk::basic_type>               var_type;
  std::vector<std::shared_ptr<cdk::basic_type>> *var_types;
};

%token <i> tINTEGER
%token <s> tIDENTIFIER tSTRING
%token <d> tDOUBLE
%token tWHILE tELSE tPRINT tPRINTLN tINPUT tBEGIN tEND tSIZEOF
%token tPUBLIC tFORWARD tFOREIGN tPRIVATE /* qualifier tokens */
%token tNULLPTR
%token tAUTO_TYPE tINT_TYPE tREAL_TYPE tSTR_TYPE tVOID_TYPE /* type tokens */
%token tSTOP tNEXT tRETURN
%token tOR tAND

%nonassoc tIFX
%nonassoc tELSE
%nonassoc tIF
%nonassoc tELIF

%right '='
%left tGE tLE tEQ tNE '>' '<'
%left '+' '-'
%left '*' '/' '%'
%nonassoc tUNARY

%type <node> stmt program elif
%type <sequence> file list exprs stmts opt_stmts vars opt_vars declarations opt_declarations
%type <expression> expr expr_assig opt_expr_assig tINT_TYPE tREAL_TYPE
%type <declaration> var declaration
%type <func_def> func_def
%type <var_type> type func_type
%type <var_types> types
%type <lvalue> lval
%type <block> block


%{
//-- The rules below will be included in yyparse, the main parsing function.
%}
%%

file      : /* empty */       { compiler->ast($$ = new cdk::sequence_node(LINE)); }
          | declarations      { compiler->ast($$ = $1); }
          | program           { compiler->ast($$ = new cdk::sequence_node(LINE, $1)); }
          | declarations      { compiler->ast($$ = new cdk::sequence_node(LINE, $2, $1)); }


program	: tBEGIN list tEND { }
	     ;

list : stmt	     { $$ = new cdk::sequence_node(LINE, $1); }
	| list stmt    { $$ = new cdk::sequence_node(LINE, $2, $1); }
	;

stmt : expr ';'                          { $$ = new mml::evaluation_node(LINE, $1); }
 	| exprs tPRINT                      { $$ = new mml::write_node(LINE, $1, false); }
 	| exprs tPRINTLN                    { $$ = new mml::write_node(LINE, $1, true); }
     | tINPUT                            { $$ = new mml::input_node(LINE); }
     | tWHILE '(' expr ')' stmt          { $$ = new mml::while_node(LINE, $3, $5); }
     | tIF '(' expr ')' stmt %prec tIFX  { $$ = new mml::if_node(LINE, $3, $5); }
     | tIF '(' expr ')' stmt elif        { $$ = new mml::if_else_node(LINE, $3, $5, $6); }
     | '{' list '}'                      { $$ = $2; }
     | tSTOP ';'                         { $$ = new mml::stop_node(LINE, 1); }
     | tSTOP tINTEGER ';'                { $$ = new mml::stop_node(LINE, $2); }
     | tNEXT ';'                         { $$ = new mml::next_node(LINE, 1); }
     | tNEXT tINTEGER ';'                { $$ = new mml::next_node(LINE, $2); }
     | tRETURN expr ';'                  { $$ = new mml::return_node(LINE, $2); }
     | block                             { $$ = $1; }
     ;

elif : tELSE stmt                          { $$ = $2; }
     | tELIF '(' expr ')' stmt %prec tIFX  { $$ = new mml::if_node(LINE, $3, $5); }
     | tELIF '(' expr ')' stmt elif        { $$ = new mml::if_else_node(LINE, $3, $5, $6); }
     ;

stmts : stmt        { $$ = new cdk::sequence_node(LINE, $1); }
      | stmts stmt  { $$ = new cdk::sequence_node(LINE, $2, $1); }
      ;


opt_stmts : /* empty */  { $$ = new cdk::sequence_node(LINE); }
          | stmts        { $$ = $1; }
          ;                 

               
exprs : expr              { $$ = new cdk::sequence_node(LINE, $1); }
      | exprs ',' expr    { $$ = new cdk::sequence_node(LINE, $3, $1); }
      ;


expr : tINTEGER                     { $$ = new cdk::integer_node(LINE, $1); }
     | tSTRING                      { $$ = new cdk::string_node(LINE, $1); }
     | '-' expr %prec tUNARY        { $$ = new cdk::neg_node(LINE, $2); }
     | expr '+' expr	           { $$ = new cdk::add_node(LINE, $1, $3); }
     | expr '-' expr	           { $$ = new cdk::sub_node(LINE, $1, $3); }
     | expr '*' expr	           { $$ = new cdk::mul_node(LINE, $1, $3); }
     | expr '/' expr	           { $$ = new cdk::div_node(LINE, $1, $3); }
     | expr '%' expr	           { $$ = new cdk::mod_node(LINE, $1, $3); }
     | expr '<' expr	           { $$ = new cdk::lt_node(LINE, $1, $3); }
     | expr '>' expr	           { $$ = new cdk::gt_node(LINE, $1, $3); }
     | expr tGE expr	           { $$ = new cdk::ge_node(LINE, $1, $3); }
     | expr tLE expr                { $$ = new cdk::le_node(LINE, $1, $3); }
     | expr tNE expr	           { $$ = new cdk::ne_node(LINE, $1, $3); }
     | expr tEQ expr	           { $$ = new cdk::eq_node(LINE, $1, $3); }
     | '(' expr ')'                 { $$ = $2; }
     | lval                         { $$ = new cdk::rvalue_node(LINE, $1); }
     | lval '=' expr                { $$ = new cdk::assignment_node(LINE, $1, $3); }
     | lval '?'                     { $$ = new mml::address_of_node(LINE, $1); }
     | expr '(' exprs ')'           { $$ = new mml::function_call_node(LINE, $1, $3); }
     | expr '(' ')'                 { $$ = new mml::function_call_node(LINE, $1); }
     | '@' '(' exprs ')'            { $$ = new mml::function_call_node(LINE, nullptr, $3); }
     | '@' '(' ')'                  { $$ = new mml::function_call_node(LINE, nullptr); }
     | '[' expr ']'                 { $$ = new mml::stack_alloc_node(LINE, $2); }
     | tNULLPTR                     { $$ = new mml::nullptr_node(LINE); }
     | func_def                     { $$ = $1; }
     | tSIZEOF '(' expr ')'         { $$ = new mml::sizeof_node(LINE, $3); }
     | '+' tINT_TYPE %prec tUNARY   { $$ = new mml::identity_node(LINE, $2); }
     | '+' tREAL_TYPE %prec tUNARY  { $$ = new mml::identity_node(LINE, $2); }
     | '-' tINT_TYPE %prec tUNARY   { $$ = new cdk::neg_node(LINE, $2); }
     | '-' tREAL_TYPE %prec tUNARY  { $$ = new cdk::neg_node(LINE, $2); }
     ;

expr_assig : '=' expr { $$ = $2; }
           ;

opt_expr_assig : /* empty */  { $$ = nullptr; }
               | expr_assig   { $$ = $1; }
               ;


type  : tINT_TYPE          { $$ = cdk::primitive_type::create(4, cdk::TYPE_INT); }
      | tREAL_TYPE         { $$ = cdk::primitive_type::create(8, cdk::TYPE_DOUBLE); }
      | tSTR_TYPE          { $$ = cdk::primitive_type::create(4, cdk::TYPE_STRING); }
      | '[' tVOID_TYPE ']' { $$ = cdk::reference_type::create(4, cdk::primitive_type::create(4, cdk::TYPE_VOID)); }
      | '[' type ']'  
      /* Checks for nested void pointers and ignores them (eg. `[[[void]]]` == `[void]`) */
      { 
          /* Case where `type` == `[...[[void]]...]`. Follows the previous rule (i.e. creates a `[void]` pointer). */
          if ($2->name() == cdk::TYPE_POINTER && cdk::reference_type::cast($2)->referenced()->name() == cdk::TYPE_VOID) {
               $$ = cdk::reference_type::create(4, cdk::primitive_type::create(4, cdk::TYPE_VOID));
          }
          /* Case where `type` == `[...[[any_type]]...]` */
          else {
               $$ = cdk::reference_type::create(4, $2);
          }
      } 
      | func_type          { $$ = $1; }
      ;
      
      
types : type           { $$ = new std::vector<std::shared_ptr<cdk::basic_type>>(); $$->push_back($1); }
      | types ',' type { $$ = $1; $$->push_back($3); }
      ;


func_type : type '<' '>'              { $$ = cdk::functional_type::create($1); }
          | type '<' types '>'        { $$ = cdk::functional_type::create(*$3, $1); }
          | tVOID_TYPE '<' '>'        { $$ = cdk::functional_type::create(cdk::primitive_type::create(4, cdk::TYPE_VOID)); }
          | tVOID_TYPE '<' types '>'  { $$ = cdk::functional_type::create(*$3, cdk::primitive_type::create(4, cdk::TYPE_VOID)); }
          ;

func_def  : '(' opt_vars ')' '-' '>' type block { $$ = new mml::function_def_node(LINE, $2, $6, $7); }
          ;
          
opt_vars : /* empty */ { $$ = new cdk::sequence_node(LINE); }
         | vars        { $$ = $1; }
         ;
         
vars : var          { $$ = new cdk::sequence_node(LINE, $1); }
     | vars ',' var { $$ = new cdk::sequence_node(LINE, $3, $1); }
     ;

var : type tIDENTIFIER  { $$ = new mml::declaration_node(LINE, tPRIVATE, *$2, nullptr, $1); }
    ;

declaration :          type tIDENTIFIER opt_expr_assig ';'   { $$ = new mml::declaration_node(LINE, tPRIVATE, *$2, $3, $1); delete $2; }
            | tPUBLIC  type tIDENTIFIER opt_expr_assig ';'   { $$ = new mml::declaration_node(LINE, tPUBLIC , *$3, $4, $2); delete $3; }
            | tFORWARD type tIDENTIFIER opt_expr_assig ';'   { $$ = new mml::declaration_node(LINE, tPUBLIC , *$3, $4, $2); delete $3; }
            | tFOREIGN type tIDENTIFIER opt_expr_assig ';'   { $$ = new mml::declaration_node(LINE, tPUBLIC , *$3, $4, $2); delete $3; }
            |          tAUTO_TYPE tIDENTIFIER expr_assig ';' { $$ = new mml::declaration_node(LINE, tPRIVATE, *$2, $3, nullptr); delete $2; }
            | tPUBLIC  tAUTO_TYPE tIDENTIFIER expr_assig ';' { $$ = new mml::declaration_node(LINE, tPUBLIC , *$3, $4, nullptr); delete $3; }
            | tFOREIGN tAUTO_TYPE tIDENTIFIER expr_assig ';' { $$ = new mml::declaration_node(LINE, tPUBLIC , *$3, $4, nullptr); delete $3; }
            | tPUBLIC  tIDENTIFIER expr_assig ';'            { $$ = new mml::declaration_node(LINE, tPUBLIC , *$2, $3, nullptr); delete $2; }
            ;    
     
declarations : declaration               { $$ = new cdk::sequence_node(LINE, $1); }
             | declarations declaration  { $$ = new cdk::sequence_node(LINE, $2, $1); }
             ;

opt_declarations : /* empty */   { $$ = new cdk::sequence_node(LINE); }
                 | declarations  { $$ = $1; }
                 ;

block : '{' opt_declarations opt_stmts '}'  { $$ = new mml::block_node(LINE, $2, $3); }
      ;

lval : tIDENTIFIER             { $$ = new cdk::variable_node(LINE, $1); delete $1; }
     | expr '[' expr ']'       { $$ = new mml::index_node(LINE, $1, $3); }
     ;
%%
