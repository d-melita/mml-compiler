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
  /* yydebug=1; set_debug(1); */

  /*-------------------------------------*/
  /* ----------- COMMENTS -------------- */
  /*-------------------------------------*/

"//".*                ; /* ignore comment */

"/*"                  yy_push_state(X_COMMENT);
<X_COMMENT>"/*"       yy_push_state(X_COMMENT);
<X_COMMENT>"*/"       yy_pop_state();
<X_COMMENT>.|\n       ; /* ignore comments */

  /*-------------------------------------*/
  /* --------- REAL NUMBERS ------------ */
  /*-------------------------------------*/

([0-9]*\.[0-9]+|[0-9]+\.[0-9]*)([eE][-+]?[0-9]+)? {
  try {
    yylval.d = std::stod(yytext); 
    return tDOUBLE;
  } catch (const std::out_of_range&) {
    yyerror("double is out of range");
  }
}     

[0-9]+([eE][+-]?[0-9]+)                          {
  try {
    yylval.d = std::stod(yytext); 
    return tDOUBLE;
  } catch (const std::out_of_range&) {
    yyerror("double is out of range");
  }
}

  /*-------------------------------------*/
  /* ----------- INTEGERS -------------- */
  /*-------------------------------------*/

0[0-7]+              {
  try { 
    yylval.i = std::stoi(yytext, NULL, 8); 
    return tINTEGER;
  } catch (const std::out_of_range& e) {
    yyerror("Integer is out of range");
  }
}
0[0-9]+              yyerror("Invalid base 8 number.");

0|[1-9][0-9]*         {
  try { 
    yylval.i = std::stoi(yytext, NULL, 10); 
    return tINTEGER;
  } catch (const std::out_of_range& e) {
    yyerror("Integer is out of range");
  }
}

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
                            unsigned long res = std::stoi(yytext, NULL, 8);
                            if (res > 255) {
                              yyerror("Invalid 8 bit number.");
                            }
                            strlit << (char)(unsigned char)res;
                            yy_pop_state();
                          }

<X_BACKSLASH>.        strlit << *yytext; yy_pop_state();

<X_STRING_SKIP>\\\"|\\\\  ;
<X_STRING_SKIP>\0         yyerror("NULL character in string");
<X_STRING_SKIP>\n         yyerror("Newline in string");
<X_STRING_SKIP>"\""       {
                            yylval.s = new std::string(strlit.str());
                            strlit.str("");
                            yy_pop_state(); yy_pop_state(); yy_pop_state();
                            return tSTRING;
                          }
<X_STRING_SKIP>.          ;

  /*---------------------------------------------------------------------*/
  /* ----------- CONSTANTS, GLOBAL SYMBOLS AND DATA TYPES -------------- */
  /*---------------------------------------------------------------------*/

"null"                 return tNULLPTR;

"public"               return tPUBLIC;
"forward"              return tFORWARD;
"foreign"              return tFOREIGN;

"auto"                 return tAUTO_TYPE;
"int"                  return tINT_TYPE;
"double"               return tREAL_TYPE;
"string"               return tSTR_TYPE;
"void"                 return tVOID_TYPE;

  /*-------------------------------------*/
  /* ----------- FUNCTIONS ------------- */
  /*-------------------------------------*/

[@{}<>]                return *yytext;
"->"                   return tARROW;

"begin"                return tBEGIN; 
"end"                  return tEND;

  /*-----------------------------------------*/
  /* ----------- INSTRUCTIONS -------------- */
  /*-----------------------------------------*/

"if"                   return tIF; 
"else"                 return tELSE;
"elif"                 return tELIF;

"while"                return tWHILE;

"!"                    return tPRINT;
"!!"                   return tPRINTLN;

"stop"                 return tSTOP;
"next"                 return tNEXT;
"return"               return tRETURN;

  /*-----------------------------------------*/
  /* ----------- EXPRESSIONS --------------- */
  /*-----------------------------------------*/
[-+*/%\[\]~?=();,]     return *yytext;

">="                   return tGE; 
"<="                   return tLE; 
"=="                   return tEQ; 
"!="                   return tNE;

"&&"                   return tAND;
"||"                   return tOR;

"input"                return tINPUT; 
"sizeof"               return tSIZEOF; 

  /*-------------------------------------*/
  /* ---------- IDENTIFIERS ------------ */
  /*-------------------------------------*/

[A-Za-z][A-Za-z0-9]*  yylval.s = new std::string(yytext); return tIDENTIFIER;

  /*-------------------------------------*/
  /* ---------- WHITE SPACE ------------ */
  /*-------------------------------------*/

[ \t\n\r]+               ; /* ignore whitespace */

.                      yyerror("Unknown character");

%%
