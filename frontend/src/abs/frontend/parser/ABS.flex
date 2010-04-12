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
  StringBuffer string = new StringBuffer();

  private Symbol sym(short id) {
    return new Symbol(id, yyline + 1, yycolumn + 1, yylength(), yytext());
  }
  private Symbol sym(short id, String text) {
    return new Symbol(id, yyline + 1, yycolumn + 1, text.length(), text);
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

%state STRING



%% // Rules
//Keywords 
<YYINITIAL> {
 "class"       { return sym(Terminals.CLASS); }
 "interface"   { return sym(Terminals.INTERFACE); }
 "extends"     { return sym(Terminals.EXTENDS); }
 "data"        { return sym(Terminals.DATA); }
 "def"         { return sym(Terminals.DEF); }
 "implements"  { return sym(Terminals.IMPLEMENTS); }
 "while"       { return sym(Terminals.WHILE); }
 "return"      { return sym(Terminals.RETURN); }
 "Fut"         { return sym(Terminals.FUT); }
 "skip"        { return sym(Terminals.SKIP); }
 "get"         { return sym(Terminals.GET); }
 "null"        { return sym(Terminals.NULL); }
 "await"       { return sym(Terminals.AWAIT); }
 "if"          { return sym(Terminals.IF); }
 "else"        { return sym(Terminals.ELSE); }
 "suspend"     { return sym(Terminals.SUSPEND); }
 "new"         { return sym(Terminals.NEW); }
 "this"        { return sym(Terminals.THIS); }
 "pair"        { return sym(Terminals.PAIR); }
 "case"        { return sym(Terminals.CASE); }
 "let"         { return sym(Terminals.LET); }
 "in"          { return sym(Terminals.IN); }
}

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

//Literals
<YYINITIAL> {
 \"            { string.setLength(0); yybegin(STRING); }
}

<STRING> {
 \"            { yybegin(YYINITIAL); 
                 return sym(Terminals.STRINGLITERAL, 
                 string.toString()); }
 [^\n\r\"\\]+  { string.append( yytext() ); }
 \\t           { string.append('\t'); }
 \\n           { string.append('\n'); }
 \\r           { string.append('\r'); }
 \\\"          { string.append('\"'); }
 \\            { string.append('\\'); }
}


.|\n          { throw new RuntimeException("Illegal character \""+yytext()+ "\" at line "+yyline+", column "+yycolumn); }
<<EOF>>       { return sym(Terminals.EOF); }
