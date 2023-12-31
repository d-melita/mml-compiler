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

  int                                            i;     /* integer value */
  std::string                                   *s;     /* symbol name or string literal */
  double                                         d;     /* double value */
  cdk::basic_node                               *node;     /* node pointer */
  cdk::sequence_node                            *sequence;
  cdk::expression_node                          *expression; /* expression nodes */
  mml::block_node                               *block;
  cdk::lvalue_node                              *lvalue;
  std::vector<std::shared_ptr<cdk::basic_type>> *var_types;
};

  /*-------------------------------------*/
  /* ------------ TOKENS --------------- */
  /*-------------------------------------*/
  
%token <i> tINTEGER
%token <s> tIDENTIFIER tSTRING
%token <d> tDOUBLE

%token tBEGIN tELSE tEND tINPUT tPRINT tPRINTLN tSIZEOF tWHILE
%token tFOREIGN tFORWARD tPRIVATE tPUBLIC /* qualifier tokens */
%token tNULLPTR tAUTO_TYPE tINT_TYPE tREAL_TYPE tSTR_TYPE tVOID_TYPE /* type tokens */
%token tNEXT tRETURN tSTOP
%token tARROW

  /*-------------------------------------*/
  /* --------- ASSOCIATIVITY ----------- */
  /*-------------------------------------*/

%nonassoc tIF tWHILE
%nonassoc tELIF
%nonassoc tELSE
%right '='
%left tOR
%left tAND
%nonassoc '~'
%left tEQ tNE
%left tGE tLE '>' '<'
%left '+' '-'
%left '*' '/' '%'
%nonassoc tUNARY
%nonassoc '(' '[' 

  /*-------------------------------------*/
  /* -------------- TYPES -------------- */
  /*-------------------------------------*/

%type <block> block program_block
%type <expression> expr expr_assig func_def opt_expr_assig literal global_opt_expr_assig global_expr_assig
%type <lvalue> lval
%type <node> declaration elif global_decl program stmt var
%type <s> string
%type <sequence> declarations exprs file global_decls opt_vars stmts vars
%type <type> auto data_type func_type opt_auto return_types var_type void_ptr_type void_type
%type <var_types> types


  /*-------------------------------------*/
  /* -------------- RULES -------------- */
  /*-------------------------------------*/

%{
//-- The rules below will be included in yyparse, the main parsing function.
%}
%%

file      : /* empty */           { compiler->ast($$ = new cdk::sequence_node(LINE)); }
          | global_decls          { compiler->ast($$ = $1); }
          | program               { compiler->ast($$ = new cdk::sequence_node(LINE, $1)); }
          | global_decls program  { compiler->ast($$ = new cdk::sequence_node(LINE, $2, $1)); }
          ;

global_decls   : global_decl ';'                { $$ = new cdk::sequence_node(LINE, $1); }
               | global_decls global_decl ';'   { $$ = new cdk::sequence_node(LINE, $2, $1); }
               ;

global_decl    : tFORWARD var_type  tIDENTIFIER                            { $$ = new mml::declaration_node(LINE, tFORWARD, *$3, nullptr, $2); delete $3; }
               | tFOREIGN func_type tIDENTIFIER                            { $$ = new mml::declaration_node(LINE, tFOREIGN, *$3, nullptr, $2); delete $3; }
               | tPUBLIC var_type   tIDENTIFIER global_opt_expr_assig      { $$ = new mml::declaration_node(LINE, tPUBLIC, *$3, $4, $2); delete $3; }
               | tPUBLIC opt_auto   tIDENTIFIER global_opt_expr_assig      { $$ = new mml::declaration_node(LINE, tPUBLIC, *$3, $4, $2); delete $3; }
               | var_type   tIDENTIFIER global_opt_expr_assig              { $$ = new mml::declaration_node(LINE, tPUBLIC, *$2, $3, $1); delete $2; }
               | opt_auto   tIDENTIFIER global_opt_expr_assig              { $$ = new mml::declaration_node(LINE, tPUBLIC, *$2, $3, $1); delete $2; }
               ;
			
global_opt_expr_assig : /* empty */               { $$ = nullptr; }
                      | global_expr_assig         { $$ = $1; }
                      ;

global_expr_assig : '=' literal   { $$ = $2; }
                  | '=' func_def  { $$ = $2; }
                  ;

program   : tBEGIN program_block tEND { $$ = new mml::function_def_node(LINE, $2); }
          ;

program_block  :  declarations stmts         { $$ = new mml::block_node(LINE, $1, $2); }
               |  declarations               { $$ = new mml::block_node(LINE, $1, nullptr); }
               |  stmts                      { $$ = new mml::block_node(LINE, nullptr, $1); }
               | /* empty */                 { $$ = new mml::block_node(LINE, nullptr, nullptr); }
               ;

block : '{' program_block '}'            { $$ = $2; }
      ;

declarations : declaration ';'              { $$ = new cdk::sequence_node(LINE, $1); }
             | declarations declaration ';' { $$ = new cdk::sequence_node(LINE, $2, $1); }
             ;

declaration : var_type tIDENTIFIER opt_expr_assig  { $$ = new mml::declaration_node(LINE, tPRIVATE, *$2, $3, $1); delete $2; }
            | auto     tIDENTIFIER expr_assig      { $$ = new mml::declaration_node(LINE, tPRIVATE, *$2, $3, $1); delete $2; }
            ;

stmts : stmt        { $$ = new cdk::sequence_node(LINE, $1); }
      | stmts stmt  { $$ = new cdk::sequence_node(LINE, $2, $1); }
      ;

stmt : expr ';'                          { $$ = new mml::evaluation_node(LINE, $1); }
	| exprs tPRINT                      { $$ = new mml::write_node(LINE, $1, false); }
     | exprs tPRINTLN                    { $$ = new mml::write_node(LINE, $1, true); }
     | tWHILE '(' expr ')' stmt          { $$ = new mml::while_node(LINE, $3, $5); }
     | tIF '(' expr ')' stmt %prec tIF   { $$ = new mml::if_node(LINE, $3, $5); }
     | tIF '(' expr ')' stmt elif        { $$ = new mml::if_else_node(LINE, $3, $5, $6); }
     | tSTOP ';'                         { $$ = new mml::stop_node(LINE, 1); }
     | tSTOP tINTEGER ';'                { $$ = new mml::stop_node(LINE, $2); }
     | tNEXT ';'                         { $$ = new mml::next_node(LINE, 1); }
     | tNEXT tINTEGER ';'                { $$ = new mml::next_node(LINE, $2); }
     | tRETURN ';'                       { $$ = new mml::return_node(LINE, nullptr); }
     | tRETURN expr ';'                  { $$ = new mml::return_node(LINE, $2); }
     | block                             { $$ = $1; }
     ;

elif : tELSE stmt                          { $$ = $2; }
     | tELIF '(' expr ')' stmt %prec tIF   { $$ = new mml::if_node(LINE, $3, $5); }
     | tELIF '(' expr ')' stmt elif        { $$ = new mml::if_else_node(LINE, $3, $5, $6); }
     ;
               
exprs : expr              { $$ = new cdk::sequence_node(LINE, $1); }
      | exprs ',' expr    { $$ = new cdk::sequence_node(LINE, $3, $1); }
      ;

expr : literal                       { $$ = $1; }
     | tINPUT                        { $$ = new mml::input_node(LINE); }
     | '+' expr %prec tUNARY         { $$ = new mml::identity_node(LINE, $2); }
     | '-' expr %prec tUNARY         { $$ = new cdk::neg_node(LINE, $2); }
     | expr '+' expr                 { $$ = new cdk::add_node(LINE, $1, $3); }
     | expr '-' expr                 { $$ = new cdk::sub_node(LINE, $1, $3); }
     | expr '*' expr                 { $$ = new cdk::mul_node(LINE, $1, $3); }
     | expr '/' expr                 { $$ = new cdk::div_node(LINE, $1, $3); }
     | expr '%' expr                 { $$ = new cdk::mod_node(LINE, $1, $3); }
     | expr '<' expr                 { $$ = new cdk::lt_node(LINE, $1, $3); }
     | expr '>' expr                 { $$ = new cdk::gt_node(LINE, $1, $3); }
     | expr tGE expr                 { $$ = new cdk::ge_node(LINE, $1, $3); }
     | expr tLE expr                 { $$ = new cdk::le_node(LINE, $1, $3); }
     | expr tNE expr                 { $$ = new cdk::ne_node(LINE, $1, $3); }
     | expr tEQ expr                 { $$ = new cdk::eq_node(LINE, $1, $3); }
     | expr tAND expr                { $$ = new cdk::and_node(LINE, $1, $3); }
     | expr tOR expr                 { $$ = new cdk::or_node(LINE, $1, $3); }
     | '~' expr                      { $$ = new cdk::not_node(LINE, $2); }
     | '(' expr ')'                  { $$ = $2; }
     | lval                          { $$ = new cdk::rvalue_node(LINE, $1); }
     | lval '=' expr                 { $$ = new cdk::assignment_node(LINE, $1, $3); }
     | lval '?'                      { $$ = new mml::address_of_node(LINE, $1); }
     | expr '(' exprs ')'            { $$ = new mml::function_call_node(LINE, $1, $3); }
     | expr '(' ')'                  { $$ = new mml::function_call_node(LINE, $1); }
     | '@' '(' exprs ')'             { $$ = new mml::function_call_node(LINE, nullptr, $3); }
     | '@' '(' ')'                   { $$ = new mml::function_call_node(LINE, nullptr); }
     | '[' expr ']'                  { $$ = new mml::stack_alloc_node(LINE, $2); }
     | func_def                      { $$ = $1; }
     | tSIZEOF '(' expr ')'          { $$ = new mml::sizeof_node(LINE, $3); }
     ;

expr_assig : '=' expr { $$ = $2; }
           ;

opt_expr_assig : /* empty */  { $$ = nullptr; }
               | expr_assig   { $$ = $1; }
               ;

auto : tAUTO_TYPE                        { $$ = nullptr; }
     ;

opt_auto : /* empty */                   { $$ = nullptr; }
         | auto                          { $$ = nullptr; }
         ;

string : tSTRING            { $$ = $1; }
       | string tSTRING     { $$ = $1; $$->append(*$2); delete $2; }

data_type  : tINT_TYPE              { $$ = cdk::primitive_type::create(4, cdk::TYPE_INT); }
           | tREAL_TYPE             { $$ = cdk::primitive_type::create(8, cdk::TYPE_DOUBLE); }
           | tSTR_TYPE              { $$ = cdk::primitive_type::create(4, cdk::TYPE_STRING); }
           | func_type              { $$ = $1; }
           | '[' data_type ']'      { $$ = cdk::reference_type::create(4, $2); }  
           ;

void_type  : tVOID_TYPE            { $$ = cdk::primitive_type::create(0, cdk::TYPE_VOID); }
           ;

void_ptr_type : '[' void_ptr_type ']'     { $$ = $2; }
              | '[' void_type ']'         { $$ = cdk::reference_type::create(4, $2); }
      
types : var_type           { $$ = new std::vector<std::shared_ptr<cdk::basic_type>>(); $$->push_back($1); }
      | types ',' var_type { $$ = $1; $$->push_back($3); }
      ;

return_types : var_type  { $$ = $1; }
             | void_type { $$ = $1; }
             ;

var_type : data_type { $$ = $1; }
              | void_ptr_type { $$ = $1; }
              ;

func_type : return_types '<' types '>'              { $$ = cdk::functional_type::create(*$3, $1); }
          | return_types '<' '>'                    { $$ = cdk::functional_type::create($1); }
          ;

func_def  : '(' opt_vars ')' tARROW return_types block { $$ = new mml::function_def_node(LINE, $2, $5, $6); }
          ;
          
opt_vars : /* empty */             { $$ = new cdk::sequence_node(LINE); }
         | vars                    { $$ = $1; }
         ;
         
vars : var                         { $$ = new cdk::sequence_node(LINE, $1); }
     | vars ',' var                { $$ = new cdk::sequence_node(LINE, $3, $1); }
     ;

var : var_type tIDENTIFIER    { $$ = new mml::declaration_node(LINE, tPRIVATE, *$2, nullptr, $1); }
    ;

lval : tIDENTIFIER             { $$ = new cdk::variable_node(LINE, $1); delete $1; }
     | expr '[' expr ']'       { $$ = new mml::index_node(LINE, $1, $3); }
     ;

literal   : tINTEGER       { $$ = new cdk::integer_node(LINE, $1); }
          | tDOUBLE        { $$ = new cdk::double_node(LINE, $1); }
          | string         { $$ = new cdk::string_node(LINE, $1); }
          | tNULLPTR       { $$ = new mml::nullptr_node(LINE); }
          ;
%%
