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
  A_fnDecl fnDecl;
  A_fnDef fnDef;
  A_varDecl varDecl;
  A_varDef varDef;
  A_rightVal rightVal;
  A_boolExpr boolExpr;
  A_arithBiOpExpr arithBiOpExpr;
  A_arithUExpr arithUExpr;
  A_fnCall fnCall;
  A_indexExpr indexExpr;
  A_arrayExpr arrayExpr;
  A_memberExpr memberExpr;
  A_boolUnit boolUnit;
  A_boolBiOpExpr boolBiOpExpr;
  A_boolUOpExpr boolUOpExpr;
  A_comExpr comExpr;
  A_leftVal leftVal;
  A_assignStmt assignStmt;
  A_rightValList rightValList;
  A_varDefScalar varDefScalar;
  A_varDefArray varDefArray;
  A_varDeclScalar varDeclScalar;
  A_varDeclArray varDeclArray;
  A_varDeclList varDeclList;
  A_paramDecl paramDecl;
  A_codeBlockStmt codeBlockStmt;
  A_ifStmt ifStmt;
  A_whileStmt whileStmt;
  A_callStmt callStmt;
  A_returnStmt returnStmt;
  A_codeBlockStmtList codeBlockStmtList;
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
%nonassoc NEG
%right NOT
%nonassoc LOWER_THAN_LP
%left LPAREN RPAREN LBRACKET RBRACKET LBRACE RBRACE DOT

/* 非终止符 */
%type <program> Program
%type <programElementList> ProgramElementList
%type <programElement> ProgramElement
%type <arithExpr> ArithExpr
%type <exprUnit> ExprUnit
%type <varDeclStmt> VarDeclStmt

%type <boolBiOpExpr> BoolBiOpExpr
%type <boolUnit> BoolUnit
%type <comExpr> ComExpr
%type <boolExpr> BoolExpr
%type <boolUOpExpr> BoolUOpExpr
%type <arrayExpr> ArrayExpr

%type <assignStmt> AssignStmt
%type <leftVal> LeftVal
%type <rightVal> RightVal
%type <memberExpr> MemberExpr

%type <fnCall> FnCall
%type <rightValList> RightValList

%type <varDecl> VarDecl
%type <varDef> VarDef
%type <type> Type

%type <varDeclList> VarDeclList
%type <varDecl> FieldDecl
%type <structDef> StructDef

%type <fnDeclStmt> FnDeclStmt
%type <paramDecl> ParamDecl
%type <fnDecl> FnDecl

%type <fnDef> FnDef
%type <codeBlockStmt> CodeBlockStmt
%type <codeBlockStmtList> CodeBlockStmtList
%type <codeBlockStmtList> CodeBlock
%type <callStmt> CallStmt
%type <returnStmt> ReturnStmt
%type <pos> ContinueStmt
%type <pos> BreakStmt

%type <arithUExpr> ArithUExpr
%type <ifStmt> IfStmt
%type <whileStmt> WhileStmt

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

// Arithmatic Expressions
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

ExprUnit: NUM {
  $$ = A_NumExprUnit($1->pos, $1->num);
} | ID {
  $$ = A_IdExprUnit($1->pos, $1->id);
} | LPAREN ArithExpr RPAREN {
  $$ = A_ArithExprUnit($1, $2);
} | FnCall {
  $$ = A_CallExprUnit($1->pos, $1);
} | ArrayExpr {
  $$ = A_ArrayExprUnit($1->pos, $1);
} | MemberExpr {
  $$ = A_MemberExprUnit($1->pos, $1);
} | ArithUExpr {
  $$ = A_ArithUExprUnit($1->pos, $1);
} ;

ArrayExpr: LeftVal LBRACKET NUM RBRACKET {
  $$ = A_ArrayExpr($1->pos, $1, A_NumIndexExpr($3->pos, $3->num));
} | LeftVal LBRACKET ID RBRACKET {
  $$ = A_ArrayExpr($1->pos, $1, A_IdIndexExpr($3->pos, $3->id));
} ;

// 负数
ArithUExpr: SUB ExprUnit %prec NEG {
  $$ = A_ArithUExpr($1, A_neg, $2);
} ;

// Condition Expressions
BoolExpr: BoolBiOpExpr {
  $$ = A_BoolBiOp_Expr($1->pos, $1);
} | BoolUnit {
  $$ = A_BoolExpr($1->pos, $1);
} ;

BoolBiOpExpr: BoolExpr OR BoolExpr {
  $$ = A_BoolBiOpExpr($1->pos, A_or, $1, $3);
} | BoolExpr AND BoolExpr {
  $$ = A_BoolBiOpExpr($1->pos, A_and, $1, $3);
} ;

BoolUnit: LPAREN ComExpr RPAREN {
  $$ = A_ComExprUnit($1, $2);
} | LPAREN BoolExpr RPAREN  {
  $$ = A_BoolExprUnit($1, $2);
} | BoolUOpExpr {
  $$ = A_BoolUOpExprUnit($1->pos, $1);
} ;

ComExpr: ExprUnit LT ExprUnit {
  $$ = A_ComExpr($1->pos, A_lt, $1, $3);
} | ExprUnit GT ExprUnit {
  $$ = A_ComExpr($1->pos, A_gt, $1, $3);
} | ExprUnit LE ExprUnit {
  $$ = A_ComExpr($1->pos, A_le, $1, $3);
} | ExprUnit GE ExprUnit {
  $$ = A_ComExpr($1->pos, A_ge, $1, $3);
} | ExprUnit EQ ExprUnit {
  $$ = A_ComExpr($1->pos, A_eq, $1, $3);
} | ExprUnit NE ExprUnit {
  $$ = A_ComExpr($1->pos, A_ne, $1, $3);
} ;

BoolUOpExpr: LPAREN NOT BoolUnit  RPAREN{
  $$ = A_BoolUOpExpr($1, A_not, $3);
} ;

// Assignment Statements
AssignStmt: LeftVal ASSIGN RightVal SEMICOLON {
  $$ = A_AssignStmt($1->pos, $1, $3);
} ;

LeftVal: ID  %prec LOWER_THAN_LP {
  $$ = A_IdExprLVal($1->pos, $1->id);
} | ArrayExpr {
  $$ = A_ArrExprLVal($1->pos, $1);
} | MemberExpr {
  $$ = A_MemberExprLVal($1->pos, $1);
} ;

MemberExpr: ID DOT ID {
  $$ = A_MemberExpr($1->pos, A_IdExprLVal($1->pos, $1->id), $3->id);
} | LeftVal DOT ID {
  $$ = A_MemberExpr($1->pos, $1, $3->id);
} ;

RightVal: ArithExpr {
  $$ = A_ArithExprRVal($1->pos, $1);
} ;

// Function Call
FnCall: ID LPAREN RightValList RPAREN {
  $$ = A_FnCall($1->pos, $1->id, $3);
} | ID LPAREN RPAREN {
  $$ = A_FnCall($1->pos, $1->id, nullptr);
} ;

RightValList: RightVal {
  $$ = A_RightValList($1, nullptr);
} | RightVal COMMA RightValList {
  $$ = A_RightValList($1, $3);
} ;

// Variable Declarations
VarDeclStmt: LET VarDecl SEMICOLON {
  $$ = A_VarDeclStmt($1, $2);
} | LET VarDef SEMICOLON {
  $$ = A_VarDefStmt($1, $2);
} ;

VarDecl: ID COLON Type {
  $$ = A_VarDecl_Scalar($1->pos, A_VarDeclScalar($1->pos, $1->id, $3));
} | ArrayExpr COLON Type{
  $$ = A_VarDecl_Array($1->pos, A_VarDeclArray($1->pos, $1->arr->u.id, $1->idx->u.num, $3));
} | ID {
  $$ = A_VarDecl_Scalar($1->pos, A_VarDeclScalar($1->pos, $1->id, nullptr));
} | ArrayExpr {
  $$ = A_VarDecl_Array($1->pos, A_VarDeclArray($1->pos, $1->arr->u.id, $1->idx->u.num, nullptr));
} ;

VarDef: ID COLON Type ASSIGN RightVal {
  $$ = A_VarDef_Scalar($1->pos, A_VarDefScalar($1->pos, $1->id, $3, $5));
} | ID ASSIGN RightVal {
  $$ = A_VarDef_Scalar($1->pos, A_VarDefScalar($1->pos, $1->id, nullptr, $3));
} | ArrayExpr COLON Type ASSIGN LBRACE RightValList RBRACE {
  $$ = A_VarDef_Array($1->pos, A_VarDefArray($1->pos, $1->arr->u.id, $1->idx->u.num, $3, $6));
} | ArrayExpr ASSIGN LBRACE RightValList RBRACE {
  $$ = A_VarDef_Array($1->pos, A_VarDefArray($1->pos, $1->arr->u.id, $1->idx->u.num, nullptr, $4));
} | ArrayExpr COLON Type ASSIGN LBRACE RBRACE {
  $$ = A_VarDef_Array($1->pos, A_VarDefArray($1->pos, $1->arr->u.id, $1->idx->u.num, $3, nullptr));
} | ArrayExpr ASSIGN LBRACE RBRACE {
  $$ = A_VarDef_Array($1->pos, A_VarDefArray($1->pos, $1->arr->u.id, $1->idx->u.num, nullptr, nullptr));
} ;

Type: INT {
  $$ = A_NativeType($1, A_intTypeKind);
} | ID {
  $$ = A_StructType($1->pos, $1->id);
} ;

// Structure
FieldDecl: ID COLON Type {
  $$ = A_VarDecl_Scalar($1->pos, A_VarDeclScalar($1->pos, $1->id, $3));
} | ArrayExpr COLON Type{
  $$ = A_VarDecl_Array($1->pos, A_VarDeclArray($1->pos, $1->arr->u.id, $1->idx->u.num, $3));
} ;

VarDeclList: FieldDecl {
  $$ = A_VarDeclList($1, nullptr);
} | FieldDecl COMMA VarDeclList{
  $$ = A_VarDeclList($1, $3);
} ;

StructDef: STRUCT ID LBRACE VarDeclList RBRACE{
  $$ = A_StructDef($1, $2->id, $4);
} ;

// Function Declaration
FnDeclStmt: FnDecl SEMICOLON {
  $$ = A_FnDeclStmt($1->pos, $1);
} ;

FnDecl: FN ID LPAREN ParamDecl RPAREN {
  $$ = A_FnDecl($1, $2->id, $4, nullptr);
} | FN ID LPAREN ParamDecl RPAREN ARROW Type {
  $$ = A_FnDecl($1, $2->id, $4, $7);
} ;

ParamDecl: VarDeclList {
  $$ = A_ParamDecl($1);
} | {
  $$ = A_ParamDecl(nullptr);
} ;

// Function Definition
FnDef: FnDecl CodeBlock{
  $$ = A_FnDef($1->pos, $1, $2);
} ;

CodeBlock: LBRACE RBRACE {
  $$ = nullptr;
} | LBRACE CodeBlockStmtList RBRACE {
  $$ = $2;
} ;

CodeBlockStmtList: CodeBlockStmt CodeBlockStmtList{
  $$ = A_CodeBlockStmtList($1, $2);
} | CodeBlockStmt {
  $$ = A_CodeBlockStmtList($1, nullptr);
} ;

CodeBlockStmt: SEMICOLON {
  $$ = A_BlockNullStmt($1);
} | VarDeclStmt {
  $$ = A_BlockVarDeclStmt($1->pos, $1);
} | AssignStmt {
  $$ = A_BlockAssignStmt($1->pos, $1);
} | CallStmt {
  $$ = A_BlockCallStmt($1->pos, $1);
} | IfStmt {
  $$ = A_BlockIfStmt($1->pos, $1);
} | WhileStmt {
  $$ = A_BlockWhileStmt($1->pos, $1);
} | ReturnStmt {
  $$ = A_BlockReturnStmt($1->pos, $1);
} | ContinueStmt {
  $$ = A_BlockContinueStmt($1);
} | BreakStmt {
  $$ = A_BlockBreakStmt($1);
} ;

ReturnStmt: RETURN RightVal SEMICOLON {
  $$ = A_ReturnStmt($1, $2);
} | RETURN SEMICOLON {
  $$ = A_ReturnStmt($1, nullptr);
} ;

ContinueStmt: CONTINUE SEMICOLON {
  $$ = $1;
} ;

BreakStmt: BREAK SEMICOLON {
  $$ = $1;
} ;

CallStmt: FnCall SEMICOLON {
  $$ = A_CallStmt($1->pos, $1);
} ;

// Control Flows
IfStmt: IF BoolUnit CodeBlock ELSE CodeBlock {
  $$ = A_IfStmt($1, $2, $3, $5);
} | IF BoolUnit CodeBlock {
  $$ = A_IfStmt($1, $2, $3, nullptr);
} ;

WhileStmt: WHILE BoolUnit CodeBlock {
  $$ = A_WhileStmt($1, $2, $3);
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


