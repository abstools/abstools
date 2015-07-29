//$Id$

// 
// Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
// This file is licensed under the terms of the Modified BSD License.
//
package abs.frontend.parser;

import beaver.Scanner;
import abs.frontend.parser.ABSSymbol;
import abs.frontend.parser.ABSParser.Terminals;

%%

%public
%final
%class ABSScanner
%extends Scanner
%unicode
%function nextToken
%type ABSSymbol
%yylexthrow Scanner.Exception
%line
%column
%char

%{
  private StringBuffer string = new StringBuffer();
  
  private int previousState = 0;

  private ABSSymbol sym(short id) {
    return new ABSSymbol(id, yyline + 1, yycolumn + 1, yylength(), yychar, yytext());
  }
  private ABSSymbol sym(short id, String text) {
    return new ABSSymbol(id, yyline + 1, yycolumn + 1, text.length(), yychar, text);
  }

  private ABSSymbol symString(String text) {
      return new ABSSymbol(Terminals.STRINGLITERAL, yyline + 1, yycolumn + 1 - text.length(), text.length(), yychar-text.length(), text);
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
// BoolLiteral = [Tt]rue | [Ff]alse

Identifier  = [:lowercase:] ([:letter:] | [:digit:] | "_")*
TypeIdentifier  = [:uppercase:] ([:letter:] | [:digit:] | "_")*
IntLiteral = 0 | [1-9][0-9]*


//Alternative, explicit definition
//Alpha = [a-zA-Z]
//Identifier = {Alpha}({Alpha} | [:digit:] | "_")*
//ID       [a-z][a-z0-9]*

%states STRING, SQL, SQLSTRING



%% // Rules
//Keywords
// Important: if a new keyword is added also add it to Keywords.java
//            to get syntax highlighting in Eclipse
<YYINITIAL> {
 "module"      { return sym(Terminals.MODULE); }
 "import"      { return sym(Terminals.IMPORT); }
 "export"      { return sym(Terminals.EXPORT); }
 "from"        { return sym(Terminals.FROM); }
 "class"       { return sym(Terminals.CLASS); }
 "interface"   { return sym(Terminals.INTERFACE); }
 "exception"   { return sym(Terminals.EXCEPTION); }
 "extends"     { return sym(Terminals.EXTENDS); }
 "data"        { return sym(Terminals.DATA); }
 "def"         { return sym(Terminals.DEF); }
 "implements"  { return sym(Terminals.IMPLEMENTS); }
 "delta"       { return sym(Terminals.DELTA); }
 "stateupdate" { return sym(Terminals.STATEUPDATE); }
 "objectupdate" { return sym(Terminals.OBJECTUPDATE); }
 "classupdate" { return sym(Terminals.CLASSUPDATE); }
 "uses"        { return sym(Terminals.USES); }
 "adds"        { return sym(Terminals.ADDS); }
 "modifies"    { return sym(Terminals.MODIFIES); }
 "removes"     { return sym(Terminals.REMOVES); }
 "hasField"    { return sym(Terminals.HASFIELD); }
 "hasMethod"   { return sym(Terminals.HASMETHOD); }
 "hasInterface" { return sym(Terminals.HASINTERFACE); }
 "productline" { return sym(Terminals.PRODUCTLINE); }
 "features"    { return sym(Terminals.FEATURES); }
 "after"       { return sym(Terminals.AFTER); }
 "when"        { return sym(Terminals.WHEN); }
 "product"     { return sym(Terminals.PRODUCT); }
 "while"       { return sym(Terminals.WHILE); }
 "return"      { return sym(Terminals.RETURN); }
 "skip"        { return sym(Terminals.SKIP); }
 "get"         { return sym(Terminals.GET); }
 "null"        { return sym(Terminals.NULL); }
 "await"       { return sym(Terminals.AWAIT); }
 "if"          { return sym(Terminals.IF); }
 "then"        { return sym(Terminals.THEN); }
 "else"        { return sym(Terminals.ELSE); }
 "try"         { return sym(Terminals.TRY); }
 "catch"       { return sym(Terminals.CATCH); }
 "finally"     { return sym(Terminals.FINALLY); }
 "suspend"     { return sym(Terminals.SUSPEND); }
 "duration"    { return sym(Terminals.DURATION); }
 "new"         { return sym(Terminals.NEW); }
 "movecogto"   { return sym(Terminals.MOVECOGTO); }
 "this"        { return sym(Terminals.THIS); }
 "core"        { return sym(Terminals.CORE); }
 "original"    { return sym(Terminals.ORIGINAL); }
 ".original"   { return sym(Terminals.DOTORIGINAL); }
 "case"        { return sym(Terminals.CASE); }
 "let"         { return sym(Terminals.LET); }
 "in"          { return sym(Terminals.IN); }
 "local"       { return sym(Terminals.LOCAL); }
 "type"        { return sym(Terminals.TYPE); }
 "assert"      { return sym(Terminals.ASSERT); }
 "builtin"     { return sym(Terminals.BUILTIN); }
 //
 "root"      { return sym(Terminals.ROOT); }
 "extension" { return sym(Terminals.EXTENSION); }
 "group"     { return sym(Terminals.GROUP); }
 "opt"       { return sym(Terminals.OPT); }
 "oneof"     { return sym(Terminals.ONEOF); }
 "allof"     { return sym(Terminals.ALLOF); }
 //"Int"       { return sym(Terminals.INT); }
 //"Bool"      { return sym(Terminals.BOOL); }
 //"in"        { return sym(Terminals.IN); }
 "ifin"      { return sym(Terminals.IFIN); }
 "ifout"     { return sym(Terminals.IFOUT); }
 "exclude"   { return sym(Terminals.EXCLUDE); }
 "require"   { return sym(Terminals.REQUIRE); }
 //"excludes"  { return sym(Terminals.EXCLUDE); }
 //"requires"  { return sym(Terminals.REQUIRE); }
 //"true"      { return sym(Terminals.TRUE); }
 //"tt"        { return sym(Terminals.TRUE); }
 //"false"     { return sym(Terminals.FALSE); }
 //"ff"        { return sym(Terminals.FALSE); }
 // (For Component Model) //
 "critical"  { return sym(Terminals.CRITICAL); }
 "port"      { return sym(Terminals.PORT);     }
 "rebind"    { return sym(Terminals.REBIND);   }
 "location"  { return sym(Terminals.LOC);      }
 "move"      { return sym(Terminals.SUBLOC);   }
 "father"    { return sym(Terminals.FATHER);   }
 //error
 "throw"      { return sym(Terminals.THROW); }
 "die"     { return sym(Terminals.DIE); }
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
 ".."          { return sym(Terminals.UNTIL); }
 "."           { return sym(Terminals.DOT); }
 "!"           { return sym(Terminals.BANG); }
 "="           { return sym(Terminals.ASSIGN); }
 "&"           { return sym(Terminals.GUARDAND); }
 "=="          { return sym(Terminals.EQEQ); }
 "!="          { return sym(Terminals.NOTEQ); }
 "=>"          { return sym(Terminals.RARROW); }
 "->"          { return sym(Terminals.IMPLIES); }
 "<->"         { return sym(Terminals.EQUIV); }
  "+"	       { return sym(Terminals.PLUS); }
  "-"          { return sym(Terminals.MINUS); }
  "*"          { return sym(Terminals.MULT); }
  "/"          { return sym(Terminals.DIV); }
  "%"          { return sym(Terminals.MOD); }
 "&&"          { return sym(Terminals.ANDAND); }
 "||"          { return sym(Terminals.OROR); }
 "|"           { return sym(Terminals.BAR); }
 "~"           { return sym(Terminals.NEGATION); }
 "<"           { return sym(Terminals.LT); }
 ">"           { return sym(Terminals.GT); }
 "<="          { return sym(Terminals.LTEQ); }
 ">="          { return sym(Terminals.GTEQ); }
 "_"           { return sym(Terminals.USCORE); }
 "'"           { return sym(Terminals.PRIME); }
}

//Literals
<YYINITIAL> {
    \"            { previousState = yystate(); yybegin(STRING); string.setLength(0);  }
    {IntLiteral}  { return sym(Terminals.INTLITERAL); }
//    {BoolLiteral} { return sym(Terminals.BOOLLITERAL); }
}

<YYINITIAL> {TypeIdentifier}  { return sym(Terminals.TYPE_IDENTIFIER); }
<YYINITIAL> {
    {Identifier}  { return sym(Terminals.IDENTIFIER); }
	{Comment}     { /* discard token */ }
	{WhiteSpace}  { /* discard token */ }
}




<STRING> {
 \\\"          { string.append('\"'); }
 \"            { yybegin(previousState);
                 return symString(string.toString()); }
 \\t           { string.append('\t'); }
 \\n           { string.append('\n'); }
 \\r           { string.append('\r'); }
 [^\n\r\"\\]+  { string.append( yytext() ); }
 \\\\          { string.append('\\'); }
}


.|\n          { return sym(Terminals.INVALID_CHARACTER); }
<<EOF>>       { return sym(Terminals.EOF); }
