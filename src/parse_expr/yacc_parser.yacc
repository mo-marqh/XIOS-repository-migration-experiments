%{
#include "filter_expr_node.hpp"
#include <string>
#include <iostream>
#include "exception.hpp"

using namespace std;
using namespace xios;

extern "C"
{
  int yyparse(void);
  int yylex(void);
  int yyerror(const char *s);
}

  IFilterExprNode* parsed;
  std::string globalInputText;
  size_t globalReadOffset = 0;

  int readInputForLexer(char* buffer, size_t* numBytesRead, size_t maxBytesToRead)
  {
    size_t numBytesToRead = maxBytesToRead;
    size_t bytesRemaining = globalInputText.length()-globalReadOffset;
    size_t i;
    if (numBytesToRead > bytesRemaining) numBytesToRead = bytesRemaining;
    for (i = 0; i < numBytesToRead; i++) buffer[i] = globalInputText.c_str()[globalReadOffset + i];
    *numBytesRead = numBytesToRead;
    globalReadOffset += numBytesToRead;
    return 0;
  }
%}

%union
{
  std::string* str;                /* symbol table index */
  xios::IScalarExprNode* scalarNode;
  xios::IFilterExprNode* filterNode;
};

%token <str> NUMBER
%token <str> VAR ID AVERAGE
%token PLUS MINUS TIMES DIVIDE POWER
%token LEFT_PARENTHESIS RIGHT_PARENTHESIS
%token <str> END

%left PLUS MINUS
%left TIMES DIVIDE
%nonassoc NEG
%right POWER

%type <scalarNode> Expression
%type <filterNode> Line Field_expr
%start Line
%%


Line:
     END            { /* Nothing to do */ }
   | Field_expr END { parsed = $1; }
  ;

Expression:
            NUMBER { $$ = new CScalarValExprNode(*$1); delete $1; }
          | VAR    { $$ = new CScalarVarExprNode(*$1); delete $1; }
          | Expression PLUS Expression   { $$ = new CScalarBinaryOpExprNode($1, "add", $3); }
          | Expression MINUS Expression  { $$ = new CScalarBinaryOpExprNode($1, "minus", $3); }
          | Expression TIMES Expression  { $$ = new CScalarBinaryOpExprNode($1, "mult", $3); }
          | Expression DIVIDE Expression { $$ = new CScalarBinaryOpExprNode($1, "div", $3); }
          | MINUS Expression %prec NEG   { $$ = new CScalarUnaryOpExprNode("neg", $2); }
          | Expression POWER Expression  { $$ = new CScalarBinaryOpExprNode($1, "pow", $3); }
          | LEFT_PARENTHESIS Expression RIGHT_PARENTHESIS    { $$ = $2; }
          | ID LEFT_PARENTHESIS Expression RIGHT_PARENTHESIS { $$ = new CScalarUnaryOpExprNode(*$1, $3); delete $1; }
          ;

Field_expr:
            ID      { $$ = new CFilterFieldExprNode(*$1); delete $1; }
          | AVERAGE { $$ = new CFilterTemporalFieldExprNode(*$1); delete $1; }
          | Field_expr PLUS Field_expr   { $$ = new CFilterFieldFieldOpExprNode($1, "add", $3); }
          | Field_expr MINUS Field_expr  { $$ = new CFilterFieldFieldOpExprNode($1, "minus", $3); }
          | Field_expr TIMES Field_expr  { $$ = new CFilterFieldFieldOpExprNode($1, "mult", $3); }
          | Field_expr DIVIDE Field_expr { $$ = new CFilterFieldFieldOpExprNode($1, "div", $3); }
          | MINUS Field_expr %prec NEG   { $$ = new CFilterUnaryOpExprNode("neg", $2); }
          | Field_expr POWER Field_expr  { $$ = new CFilterFieldFieldOpExprNode($1, "pow", $3); }
          | LEFT_PARENTHESIS Field_expr RIGHT_PARENTHESIS	{ $$ = $2; }
          | Field_expr PLUS Expression   { $$ = new CFilterFieldScalarOpExprNode($1, "add", $3); }
          | Expression PLUS Field_expr   { $$ = new CFilterScalarFieldOpExprNode($1, "add", $3); }
          | Field_expr MINUS Expression  { $$ = new CFilterFieldScalarOpExprNode($1, "minus", $3); }
          | Expression MINUS Field_expr  { $$ = new CFilterScalarFieldOpExprNode($1, "minus", $3); }
          | Field_expr TIMES Expression  { $$ = new CFilterFieldScalarOpExprNode($1, "mult", $3); }
          | Expression TIMES Field_expr  { $$ = new CFilterScalarFieldOpExprNode($1, "mult", $3); }
          | Field_expr DIVIDE Expression { $$ = new CFilterFieldScalarOpExprNode($1, "div", $3); }
          | Expression DIVIDE Field_expr { $$ = new CFilterScalarFieldOpExprNode($1, "div", $3); }
          | Field_expr POWER Expression  { $$ = new CFilterFieldScalarOpExprNode($1, "pow", $3); }
          | ID LEFT_PARENTHESIS Field_expr RIGHT_PARENTHESIS { $$ = new CFilterUnaryOpExprNode(*$1, $3); delete $1; }
          ;
%%

extern "C"
{
  int yyerror(const char *s)
  {
    ERROR("int yyerror(const char *s)", << "Parsing error: " << s << endl);
  }
}

namespace xios
{
  IFilterExprNode* parseExpr(const string& strExpr)
  {
    globalInputText = strExpr;
    globalReadOffset = 0;
    yyparse();
    return parsed;
  }
}


