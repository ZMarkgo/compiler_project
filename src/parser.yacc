%{
#include <stdio.h>
#include "TeaplAst.h"

extern A_pos pos;
extern A_program root;

extern int yylex(void);
extern "C" {
extern void yyerror(const char *s); 
extern int  yywrap();
}

%}

// TODO:
// your parser

%union {
  A_pos pos;
  A_tokenId tokenId;
  A_tokenNum tokenNum;
  A_type type;
  A_program program;
  A_programElementList programElementList;
  A_programElement programElement;
  A_arithExpr arithExpr;
  A_exprUnit exprUnit;
  A_structDef structDef;
  A_varDeclStmt varDeclStmt;
  A_fnDeclStmt fnDeclStmt;
  A_fnDef fnDef;
}

/* 终止符 */
%token <pos> ADD SUB MUL DIV
// ( ) [ ] { }
%token <pos> LPAREN RPAREN LBRACKET RBRACKET LBRACE RBRACE
// , ;
%token <pos> COMMA SEMICOLON
// . -> :
%token <pos> DOT ARROW COLON
// '='
%token <pos> ASSIGN
// '==' '!=' '<=' '>=' '<' '>' 
%token <pos> EQ NE LE GE LT GT
// '&&' '||' '!'
%token <pos> AND OR NOT
// if else while continue break return
%token <pos> IF ELSE WHILE CONTINUE BREAK RETURN
// int
%token <pos> INT
// fn
%token <pos> FN
// let
%token <pos> LET
// struct
%token <pos> STRUCT

%token <tokenNum> NUM
%token <tokenId> ID

// 优先级：从低到高
%left COMMA
%right ASSIGN
%left OR
%left AND 
%left EQ NE
%left LT GT LE GE 
%left ADD SUB
%left MUL DIV
%right NOT
%left LPAREN RPAREN LBRACKET RBRACKET LBRACE RBRACE

/* 非终止符 */
%type <program> Program
%type <programElementList> ProgramElementList
%type <programElement> ProgramElement
%type <arithExpr> ArithExpr
%type <exprUnit> ExprUnit
%type <structDef> StructDef
%type <varDeclStmt> VarDeclStmt
%type <fnDeclStmt> FnDeclStmt
%type <fnDef> FnDef

%start Program

%% /* beginning of rules section */

Program: ProgramElementList {  
  root = A_Program($1);
  $$ = A_Program($1);
} ;

ProgramElementList: ProgramElement ProgramElementList {
  $$ = A_ProgramElementList($1, $2);
} | /*empty*/{
  $$ = nullptr;
} ;

ProgramElement: VarDeclStmt {
  $$ = A_ProgramVarDeclStmt($1->pos, $1);
} | StructDef {
  $$ = A_ProgramStructDef($1->pos, $1);
} | FnDeclStmt {
  $$ = A_ProgramFnDeclStmt($1->pos, $1);
} | FnDef {
  $$ = A_ProgramFnDef($1->pos, $1);
} | SEMICOLON {
  $$ = A_ProgramNullStmt($1);
} ;


ArithExpr: ArithExpr ADD ArithExpr {
  $$ = A_ArithBiOp_Expr($1->pos, A_ArithBiOpExpr($1->pos, A_add, $1, $3));
} | ArithExpr SUB ArithExpr {
  $$ = A_ArithBiOp_Expr($1->pos, A_ArithBiOpExpr($1->pos, A_sub, $1, $3));
} | ArithExpr MUL ArithExpr {
  $$ = A_ArithBiOp_Expr($1->pos, A_ArithBiOpExpr($1->pos, A_mul, $1, $3));
} | ArithExpr DIV ArithExpr {
  $$ = A_ArithBiOp_Expr($1->pos, A_ArithBiOpExpr($1->pos, A_div, $1, $3));
} | ExprUnit {
  $$ = A_ExprUnit($1->pos, $1);
} ;

%%

extern "C" {
void yyerror(const char * s) {
  fprintf(stderr, "%s\n",s);
}
int yywrap() {
  return(1);
}
}


