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
Identifier  = [:lowercase:] ([:letter:] | [:digit:] | "_")*																 
TypeIdentifier  = [:uppercase:] ([:letter:] | [:digit:] | "_")* 																 
//Identifier     = [:letter:] ([:letter:] | [:digit:] | "_")*



//Alternative, explicit definition 
//Alpha = [a-zA-Z]
//Identifier = {Alpha}({Alpha} | [:digit:] | "_")*
//ID       [a-z][a-z0-9]*



%% // Rules
//Keywords 
<YYINITIAL> {
 "class"       { return sym(Terminals.CLASS); }
 "interface"   { return sym(Terminals.INTERFACE); }
 "extends"   { return sym(Terminals.EXTENDS); }
 "data"        { return sym(Terminals.DATA); }
 "def"        { return sym(Terminals.DEF); }
 "implements"  { return sym(Terminals.IMPLEMENTS); }
 "while"       { return sym(Terminals.WHILE); }
 "return"      { return sym(Terminals.RETURN); }
 "Fut"         { return sym(Terminals.FUT); }
 "skip"        { return sym(Terminals.SKIP); }
 "get"         { return sym(Terminals.GET); }
 "null"        { return sym(Terminals.NULL); }
 "await"       { return sym(Terminals.AWAIT); }
 "if"          { return sym(Terminals.IF); }
// "then"        { return sym(Terminals.THEN); }
 "else"        { return sym(Terminals.ELSE); }
 "suspend"     { return sym(Terminals.SUSPEND); }
 "new"         { return sym(Terminals.NEW); }
 "this"         { return sym(Terminals.THIS); }
}

// "true"        { return sym(Terminals.BOOLEAN_LITERAL); }
// "false"       { return sym(Terminals.BOOLEAN_LITERAL); }
// "bool"        { return sym(Terminals.BOOL); }

//Separators
<YYINITIAL> {
 "("           { return sym(Terminals.LPAREN); }
 ")"           { return sym(Terminals.RPAREN); }
 "{"           { return sym(Terminals.LBRACE); }
 "}"           { return sym(Terminals.RBRACE); }
 ","           { return sym(Terminals.COMMA); }
 ";"           { return sym(Terminals.SEMICOLON); }
}

//Operators
<YYINITIAL> { 
 "?"           { return sym(Terminals.QMARK); }
 "."           { return sym(Terminals.DOT); }
 "!"           { return sym(Terminals.BANG); }
 "="           { return sym(Terminals.ASSIGN); }
 "&"           { return sym(Terminals.GUARDAND); }
 "=="          { return sym(Terminals.EQEQ); }
 "=>"          { return sym(Terminals.RARROW); }
}

{Comment}     { /* discard token */ }
{WhiteSpace}  { /* discard token */ }
//{LCIdentifier}  { return sym(Terminals.LCIDENTIFIER); }


<YYINITIAL> {
    {TypeIdentifier}  { return sym(Terminals.TYPEIDENTIFIER); }	
    {Identifier}  { return sym(Terminals.IDENTIFIER); }
}

//An identifier with a trailing paren is a method identifier. 
//{Identifier} / [ \t\f]* "("  { return sym(Terminals.PARENIDENTIFIER); }



.|\n          { throw new RuntimeException("Illegal character \""+yytext()+ "\" at line "+yyline+", column "+yycolumn); }
<<EOF>>       { return sym(Terminals.EOF); }
