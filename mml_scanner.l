%option c++ prefix="mml_scanner_" outfile="mml_scanner.cpp"
%option stack noyywrap yylineno 8bit debug
%{ 
// make relevant includes before including the parser's tab file
#include <string>
#include <cdk/ast/sequence_node.h>
#include <cdk/ast/expression_node.h>
#include <cdk/ast/lvalue_node.h>
#include "mml_parser.tab.h"

// don't change this
#define yyerror LexerError
%}
%x X_STRING
%%
  yydebug=1; set_debug(1);

"--".*                 ; /* ignore comments */

">="                   return tGE; 
"<="                   return tLE; 
"=="                   return tEQ; 
"!="                   return tNE; 

"while"                return tWHILE; 
"if"                   return tIF; 
"else"                 return tELSE; 
"!"                    return tPRINT;
"!!"                   return tPRINTLN;
"input"                return tINPUT;
"begin"                return tBEGIN; 
"end"                  return tEND; 
"sizeof"               return tSIZEOF; 
"null"                 return tNULLPTR;

"public"               return tPUBLIC;
"forward"              return tFORWARD;
"foreign"              return tFOREIGN;

"auto"                 return tAUTO;
"int"                  return tINT_TYPE;
"double"               return tREAL_TYPE;
"string"               return tSTR_TYPE;
"void"                 return tVOID_TYPE;

"stop"                 return tSTOP;
"next"                 return tNEXT;
"return"               return tRETURN;


[A-Za-z][A-Za-z0-9_]*  yylval.s = new std::string(yytext); return tIDENTIFIER;
\'                     yy_push_state(X_STRING); yylval.s = new std::string("");
<X_STRING>\'           yy_pop_state(); return tSTRING;
<X_STRING>\\\'         *yylval.s += yytext + 1;
<X_STRING>.            *yylval.s += yytext;
<X_STRING>\n           yyerror("newline in string");

[0-9]+                                           yylval.i = strtol(yytext, nullptr, 10); return tINTEGER;
( ([0-9]+)?\.([0-9]*)? | ([0-9]*)?\.([0-9]+)? )  yylval.d = strtod(yytext, NULL); return tREAL;

[-()<>=+*/%;{}.]       return *yytext;

[ \t\n]+               ; /* ignore whitespace */

.                      yyerror("Unknown character");

%%
