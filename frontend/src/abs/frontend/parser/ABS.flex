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


IntLiteral = 0 | [1-9][0-9]*

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
 "skip"        { return sym(Terminals.SKIP); }
 "get"         { return sym(Terminals.GET); }
 "null"        { return sym(Terminals.NULL); }
 "await"       { return sym(Terminals.AWAIT); }
 "if"          { return sym(Terminals.IF); }
 "else"        { return sym(Terminals.ELSE); }
 "suspend"     { return sym(Terminals.SUSPEND); }
 "new"         { return sym(Terminals.NEW); }
 "this"        { return sym(Terminals.THIS); }
 "case"        { return sym(Terminals.CASE); }
 "let"         { return sym(Terminals.LET); }
 "in"          { return sym(Terminals.IN); }
 "cog"         { return sym(Terminals.COG); }
 "type"        { return sym(Terminals.TYPE); }
 "assert"      { return sym(Terminals.ASSERT); }
}

//Separators
<YYINITIAL> {
 "("           { return sym(Terminals.LPAREN); }
 ")"           { return sym(Terminals.RPAREN); }
 "{"           { return sym(Terminals.LBRACE); }
 "}"           { return sym(Terminals.RBRACE); }
 "["           { return sym(Terminals.LBRACKET); }
 "]"           { return sym(Terminals.RBRACKET); }
 ","           { return sym(Terminals.COMMA); }
 ";"           { return sym(Terminals.SEMICOLON); }
 ":"           { return sym(Terminals.COLON); }
}

//Operators
<YYINITIAL> { 
 "?"           { return sym(Terminals.QMARK); }
 "."           { return sym(Terminals.DOT); }
 "!"           { return sym(Terminals.BANG); }
 "="           { return sym(Terminals.ASSIGN); }
 "&"           { return sym(Terminals.GUARDAND); }
 "=="          { return sym(Terminals.EQEQ); }
 "!="          { return sym(Terminals.NOTEQ); }
 "=>"          { return sym(Terminals.RARROW); }
  "+"	       { return sym(Terminals.PLUS); }
  "-"          { return sym(Terminals.MINUS); }
  "*"          { return sym(Terminals.MULT); }
  "/"          { return sym(Terminals.DIV); }
  "%"          { return sym(Terminals.MOD); }
 "&&"          { return sym(Terminals.ANDAND); }
 "||"          { return sym(Terminals.OROR); }
 "|"          { return sym(Terminals.BAR); }
 "~"          { return sym(Terminals.NEGATION); }
 "<"          { return sym(Terminals.LT); }
 ">"          { return sym(Terminals.GT); }
 "<="          { return sym(Terminals.LTEQ); }
 ">="          { return sym(Terminals.GTEQ); }
 "_"          { return sym(Terminals.USCORE); }
}

{Comment}     { /* discard token */ }
{WhiteSpace}  { /* discard token */ }

<YYINITIAL> {
    {TypeIdentifier}  { return sym(Terminals.TYPEIDENTIFIER); }	
    {Identifier}  { return sym(Terminals.IDENTIFIER); }
}

//Literals
<YYINITIAL> {
 \"            { string.setLength(0); yybegin(STRING); }
 {IntLiteral}  { return sym(Terminals.INTLITERAL); }
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


.|\n          { throw new SyntaxError("Illegal character \""+yytext()+ "\" at line "+yyline+", column "+yycolumn); }
<<EOF>>       { return sym(Terminals.EOF); }
