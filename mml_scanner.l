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

  /* REAL NUMBERS */
[0-9]*\.[0-9]+([Ee][-+]?[0-9]+)?          yylval.d = std::stod(yytext); return tREAL_TYPE;
[0-9]+([Ee][+-]?[0-9]+)                   yylval.d = std::stod(yytext); return tREAL_TYPE;

  /* INTEGERS */
0[0-7]+ {
  try {
    yy.lval.i = std::stoul(yytext, NULL, 8); return tINT_TYPE;
  } catch (const std::out_of_range& e) {
    yyerror("Integer out of range);
  }
}

0[0-9]+                                   yyerrror("Invalid base 8 number");
0|[1-9][0-9]* {
  try {
    yy.lval.i = std::stoul(yytext, NULL, 10); return tINT_TYPE;
  } catch (const std::out_of_range& e) {
    yyerror("Integer out of range);
  }
}


[-()<>=+*/%;{}.]       return *yytext;

[ \t\n]+               ; /* ignore whitespace */

.                      yyerror("Unknown character");

%%
