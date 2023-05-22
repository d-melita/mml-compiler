%option c++ prefix="mml_scanner_" outfile="mml_scanner.cpp"
%option stack noyywrap yylineno 8bit debug
%{ 
// make relevant includes before including the parser's tab file
#include <string>
#include <sstream>
#include <cdk/ast/sequence_node.h>
#include <cdk/ast/expression_node.h>
#include <cdk/ast/lvalue_node.h>
#include "mml_parser.tab.h"

static std::ostringstream strlit;

// don't change this
#define yyerror LexerError
%}
%x X_STRING X_BACKSLASH X_COMMENT X_STRING_SKIP
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
"sizeof"               return tSIZEOF; 
"null"                 return tNULLPTR;

"public"               return tPUBLIC;
"forward"              return tFORWARD;
"foreign"              return tFOREIGN;

"auto"                 return tAUTO_TYPE;
"int"                  return tINT_TYPE;
"double"               return tREAL_TYPE;
"string"               return tSTR_TYPE;
"void"                 return tVOID_TYPE;

"stop"                 return tSTOP;
"next"                 return tNEXT;
"return"               return tRETURN;

  /*-------------------------------------*/
  /* ----------- COMMENTS -------------- */
  /*-------------------------------------*/

"//".*\n              { yyless(yyleng - 1); }

"/*"                  yy_push_state(X_COMMENT);
<X_COMMENT>"/*"       yy_push_state(X_COMMENT);
<X_COMMENT>"*/"       yy_pop_state();
<X_COMMENT>.|\n       ;

  /*-------------------------------------*/
  /* ----------- FUNCTIONS ------------- */
  /*-------------------------------------*/

[():@,]               return *yytext;
"begin"                return tBEGIN; 
"end"                  return tEND;

  /*-------------------------------------*/
  /* ---------- IDENTIFIERS ------------ */
  /*-------------------------------------*/

[A-Za-z]([A-Za-z]|[[:digit:]])*  yylval.s = new std::string(yytext); return tIDENTIFIER;
  
  /*-------------------------------------*/
  /* ------------ STRINGS -------------- */
  /*-------------------------------------*/

"\""                   yy_push_state(X_STRING);
<X_STRING>\\           yy_push_state(X_BACKSLASH);
<X_STRING>"\""        {
                        yylval.s = new std::string(strlit.str());
                        strlit.str("");
                        yy_pop_state();
                        return tSTRING;  
                      }
  

<X_STRING>"...\n"     ;
<X_STRING>\0          yyerror("NULL character in string");
<X_STRING>\n          yyerror("Newline in string");
<X_STRING>.           strlit << *yytext;

<X_BACKSLASH>t        strlit << '\t'; yy_pop_state();
<X_BACKSLASH>n        strlit << '\n'; yy_pop_state();
<X_BACKSLASH>r        strlit << '\r'; yy_pop_state();
<X_BACKSLASH>\"       strlit << '\"'; yy_pop_state();
<X_BACKSLASH>\\       strlit << '\\'; yy_pop_state();
<X_BACKSLASH>0        yy_push_state(X_STRING_SKIP);

<X_BACKSLASH>[0-7]{1,3}   {
                            unsigned long res = std::stoul(yytext, NULL, 8);
                            if (res > 255) {
                              yyerror("Invalid 8 bit number.");
                            }
                            strlit << (char)(unsigned char)res;
                            yy_pop_state();
                          }

<X_BACKSLASH>.        strlit << *yytext; yy_pop_state();

<X_STRING_SKIP>"\""   {
                        yylval.s = new std::string(strlit.str());
                        strlit.str("");
                        yy_pop_state(); yy_pop_state(); yy_pop_state();
                        return tSTRING;
                      }
<X_STRING_SKIP>.      ;

  /*-------------------------------------*/
  /* --------- REAL NUMBERS ------------ */
  /*-------------------------------------*/

[0-9]*\.[0-9]+([Ee][-+]?[0-9]+)?          yylval.d = std::stod(yytext); return tREAL_TYPE;
[0-9]+([Ee][+-]?[0-9]+)                   yylval.d = std::stod(yytext); return tREAL_TYPE;

  /*-------------------------------------*/
  /* ----------- INTEGERS -------------- */
  /*-------------------------------------*/

0[0-7]+ {
  try {
    yylval.i = std::stoul(yytext, NULL, 8); return tINT_TYPE;
  } catch (const std::out_of_range& e) {
    yyerror("Integer out of range");
  }
}
0[0-9]+                                   yyerror("Invalid base 8 number");

0|[1-9][0-9]* {
  try {
    yylval.i = std::stoul(yytext, NULL, 10); return tINT_TYPE;
  } catch (const std::out_of_range& e) {
    yyerror("Integer out of range");
  }
}


[-()<>=+*/%;{}.]       return *yytext;

[ \t\n\r]+               ; /* ignore whitespace */

.                      yyerror("Unknown character");

%%
