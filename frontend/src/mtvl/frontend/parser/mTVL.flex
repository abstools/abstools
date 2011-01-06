//$Id$ 
package mtvl.frontend.parser;

import beaver.Symbol;
import beaver.Scanner;
import mtvl.frontend.parser.MTVLParser.Terminals;
import abs.frontend.parser.LexicalError;
import abs.frontend.parser.ParseException;

%%

%public
%final
%class MTVLScanner
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
//    System.out.print(""+id+"-");
    return new Symbol(id, yyline + 1, yycolumn + 1, yylength(), yytext());
  }
  private Symbol sym(short id, String text) {
    return new Symbol(id, yyline + 1, yycolumn + 1, text.length(), text);
  }
%}


// Helper Definitions

LineTerminator = \r|\n|\r\n
InputCharacter = [^\r\n]
WhiteSpace     = {LineTerminator} | [ \t\f]


//Comments
Comment = {TraditionalComment}  | {EndOfLineComment}
  
TraditionalComment = "/*" [^*] ~"*/" | "/*" "*"+ "/" | "/*" "*"+ [^/*] ~"*/"
EndOfLineComment   = "//" {InputCharacter}* {LineTerminator}?


//Identifiers defined using character classes 
AttIdentifier  = ([:lowercase:] ([:letter:] | [:digit:] | "_")*)
//               | ([:uppercase:] ([:letter:] | [:digit:] | "_" | ".")* "."
//                  [:lowercase:] ([:letter:] | [:digit:] | "_")*)
FeatIdentifier = [:uppercase:] ([:letter:] | [:digit:] | "_")*

IntLiteral = 0 | [1-9][0-9]*


// % state STRING



%%

// Keywords 
<YYINITIAL> {
 "root"      { return sym(Terminals.ROOT); }
 "extension" { return sym(Terminals.EXTENSION); }
 "group"     { return sym(Terminals.GROUP); }
 "opt"       { return sym(Terminals.OPT); }
 "oneof"     { return sym(Terminals.ONEOF); }
 "allof"     { return sym(Terminals.ALLOF); }
 "Int"       { return sym(Terminals.INT); }
 "Bool"      { return sym(Terminals.BOOL); }
 "in"        { return sym(Terminals.IN); }
 "ifin"      { return sym(Terminals.IFIN); }
 "ifout"     { return sym(Terminals.IFOUT); }
 "exclude"   { return sym(Terminals.EXCLUDE); }
 "require"   { return sym(Terminals.REQUIRE); }
 "excludes"  { return sym(Terminals.EXCLUDE); }
 "requires"  { return sym(Terminals.REQUIRE); }
 "true"      { return sym(Terminals.TRUE); }
 "tt"        { return sym(Terminals.TRUE); }
 "false"     { return sym(Terminals.FALSE); }
 "ff"        { return sym(Terminals.FALSE); }
}


// Separators
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
 ".."          { return sym(Terminals.UNTIL); }
 "."           { return sym(Terminals.DOT); }
}

// Operators
<YYINITIAL> { 
// "!"      { return sym(Terminals.BANG); }
// "="      { return sym(Terminals.ASSIGN); }
 "&&"     { return sym(Terminals.AND); }
 "=="     { return sym(Terminals.EQ); }
 "!="     { return sym(Terminals.NOTEQ); }
 "->"     { return sym(Terminals.RARROW); }
 "<->"    { return sym(Terminals.LRARROW); }
  "+"     { return sym(Terminals.PLUS); }
  "-"     { return sym(Terminals.MINUS); }
  "*"     { return sym(Terminals.MULT); }
  "/"     { return sym(Terminals.DIV); }
  "%"     { return sym(Terminals.MOD); }
 "||"     { return sym(Terminals.OR); }
 "~"      { return sym(Terminals.NEGATION); }
 "!"      { return sym(Terminals.NEGATION); }
 "not"    { return sym(Terminals.NEGATION); }
 "<"      { return sym(Terminals.LT); }
 ">"      { return sym(Terminals.GT); }
 "<="     { return sym(Terminals.LTEQ); }
 ">="     { return sym(Terminals.GTEQ); }
// "_"      { return sym(Terminals.USCORE); }
}

// Identifiers & literals
<YYINITIAL> {
  {AttIdentifier}   { return sym(Terminals.AID); }
  {FeatIdentifier}  { return sym(Terminals.FID); }
  {IntLiteral}      { return sym(Terminals.INTLIT); }
  {Comment}         { /* discard token */ }
  {WhiteSpace}      { /* discard token */ }
}


.|\n          { throw new ParseException(new LexicalError("Illegal character \""+yytext()+"\"",yyline+1,yycolumn+1)); }
<<EOF>>       { return sym(Terminals.EOF); }