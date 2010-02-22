//$Id$ 
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


//Identifiers defined using character classes 
//LCIdentifier  = [:lower:] ([:letter:] | [:digit:] | "_")*																 
//UCIdentifier  = [:upper:] ([:letter:] | [:digit:] | "_")*																 
Identifier     = [:letter:] ([:letter:] | [:digit:] | "_")*

//Identifier = [:letter:]([:letter:] | [:digit:])*


//Alternative, explicit definition 
//Alpha = [a-zA-Z]
//Identifier = {Alpha}({Alpha} | [:digit:] | "_")*
//ID       [a-z][a-z0-9]*



%% // Rules

 "class"       { return sym(Terminals.CLASS); }
 "interface"   { return sym(Terminals.INTERFACE); }
 "extends"   { return sym(Terminals.EXTENDS); }
 "data"        { return sym(Terminals.DATA); }
 "def"        { return sym(Terminals.DEF); }
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

// "true"        { return sym(Terminals.BOOLEAN_LITERAL); }
// "false"       { return sym(Terminals.BOOLEAN_LITERAL); }
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
 "P"           { return sym(Terminals.PAIR); }
"&"          { return sym(Terminals.GUARDAND); }
//Logical operators 
// "~"          { return sym(Terminals.NEG); }
// "&&"          { return sym(Terminals.ANDAND); }
// "||"          { return sym(Terminals.OROR); }
//"=="          { return sym(Terminals.EQEQ); }

{Comment}     { /* discard token */ }
{WhiteSpace}  { /* discard token */ }
//{LCIdentifier}  { return sym(Terminals.LCIDENTIFIER); }
//{UCIdentifier}  { return sym(Terminals.UCIDENTIFIER); }
{Identifier}  { return sym(Terminals.IDENTIFIER); }


.|\n          { throw new RuntimeException("Illegal character \""+yytext()+ "\" at line "+yyline+", column "+yycolumn); }
<<EOF>>       { return sym(Terminals.EOF); }
