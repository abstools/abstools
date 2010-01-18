package abs.frontend.parser;

import beaver.Symbol;
import beaver.Scanner;
import abs.frontend.parser.ABSParser.Terminals;

%%

%public
%final
%class ABSScanner
%extends Scanner
%unicode
%function nextToken
%type Symbol
%yylexthrow Scanner.Exception
%line
%column

%{
  private Symbol sym(short id) {
    return new Symbol(id, yyline + 1, yycolumn + 1, yylength(), yytext());
  }
%}

// Helper Definitions

LineTerminator = \r|\n|\r\n
InputCharacter = [^\r\n]

WhiteSpace = {LineTerminator} | [ \t\f]


//Comments
Comment = {TraditionalComment}	| {EndOfLineComment}
	
TraditionalComment = "/*" [^*] ~"*/" | "/*" "*"+ "/" | "/*" "*"+ [^/*] ~"*/"
EndOfLineComment = "//" {InputCharacter}* {LineTerminator}?


																  //Comment = "//" {InputCharacter}* {LineTerminator}?   // TODO: C-style comments

Identifier = [:letter:]([:letter:] | [:digit:])*

%% // Rules

//temporary for development
// "msig"       { return sym(Terminals.MSIG); }


 "class"       { return sym(Terminals.CLASS); }
 "interface"   { return sym(Terminals.INTERFACE); }
 "data"        { return sym(Terminals.DATA); }
 "implements"  { return sym(Terminals.IMPLEMENTS); }
// "while"       { return sym(Terminals.WHILE); }
 "return"      { return sym(Terminals.RETURN); }
 "fut"         { return sym(Terminals.FUT); }
 "skip"        { return sym(Terminals.SKIP); }
 "get"         { return sym(Terminals.GET); }
 "null"        { return sym(Terminals.NULL); }
 "await"       { return sym(Terminals.AWAIT); }
 "if"          { return sym(Terminals.IF); }
 "then"        { return sym(Terminals.THEN); }
 "else"        { return sym(Terminals.ELSE); }
 "release"     { return sym(Terminals.RELEASE); }
 "new"         { return sym(Terminals.NEW); }

 "true"        { return sym(Terminals.BOOLEAN_LITERAL); }
 "false"       { return sym(Terminals.BOOLEAN_LITERAL); }
// "bool"        { return sym(Terminals.BOOL); }

 "("           { return sym(Terminals.LPAREN); }
 ")"           { return sym(Terminals.RPAREN); }
 "{"           { return sym(Terminals.LBRACE); }
 "}"           { return sym(Terminals.RBRACE); }
 ","           { return sym(Terminals.COMMA); }
 ";"           { return sym(Terminals.SEMICOLON); }
 "?"           { return sym(Terminals.QMARK); }
 "."           { return sym(Terminals.DOT); }
 "!"           { return sym(Terminals.BANG); }
 "="           { return sym(Terminals.ASSIGN); }
"&"          { return sym(Terminals.GUARDAND); }
//Logical operators 
 "~"          { return sym(Terminals.NEG); }
 "&&"          { return sym(Terminals.ANDAND); }
 "||"          { return sym(Terminals.OROR); }
 "=="          { return sym(Terminals.EQEQ); }

{Comment}     { /* discard token */ }
{WhiteSpace}  { /* discard token */ }
{Identifier}  { return sym(Terminals.IDENTIFIER); }

.|\n          { throw new RuntimeException("Illegal character \""+yytext()+ "\" at line "+yyline+", column "+yycolumn); }
<<EOF>>       { return sym(Terminals.EOF); }
