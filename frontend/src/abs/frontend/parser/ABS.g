//$Id$

grammar ABS;

options {
	output = AST;
	ASTLabelType=CommonTree;
}

tokens {


PROGRAM;
FIELDS;
METHODS;
METHSIGN ;
PARAMS ; 
METHOD ; 
VARS; 
DECL;
STMTS;
STMLIST ; 
ASS ; 
}


@parser::header { 
package abs.frontend.parser;
}

@lexer::header { 
package abs.frontend.parser;
}

@members {
public static void main(String[] args) throws Exception {

    boolean treeflag = false;
	ABSLexer lex = new ABSLexer(new ANTLRFileStream(args[0]));
	CommonTokenStream tokens = new CommonTokenStream(lex);
	ABSParser parser = new ABSParser(tokens);
    int argind ; 
    //Get command line options.
    for (argind=0 ; argind < args.length ; argind++){
        if (args[argind].equals("-tree"))
            treeflag = true; 
    }        
    try {
        //    	parser.program();
   	    ABSParser.program_return r = parser.program(); 
         // print tree if building trees
        if ( treeflag && r!=null ) {
            //System.out.println(r); 
                        System.out.println();
            System.out.println(((CommonTree)r.tree).toStringTree());
            System.out.println();
            printTree((CommonTree)r.tree, 2 );
        } else 
            System.out.println("Parse OK");
    } catch (RecognitionException e)  {
		e.printStackTrace();
	}
}

public static void printTree(CommonTree t, int indent) {
	if ( t != null ) {
		StringBuffer sb = new StringBuffer(indent);
		for ( int i = 0; i < indent; i++ )
			sb = sb.append("   ");
		for ( int i = 0; i < t.getChildCount(); i++ ) {
			System.out.println(sb.toString() + t.getChild(i).toString());
			printTree((CommonTree)t.getChild(i), indent+1);
		}
	}
}

public String getErrorMessage(RecognitionException e,
                                String[] tokenNames)
{
    List stack = getRuleInvocationStack(e, this.getClass().getName());
    String msg = null;
    if ( e instanceof NoViableAltException ) {
        NoViableAltException nvae = (NoViableAltException)e;
        msg = " no viable alt; \ntoken="+e.token+
            " (decision="+nvae.decisionNumber+
            " state "+nvae.stateNumber+")"+
            " \ndecision=<<"+nvae.grammarDecisionDescription+">>";
    }
    else     {
        msg = super.getErrorMessage(e, tokenNames);
    }
    return stack+" "+msg;
}

public String getTokenErrorDisplay(Token t) {
    return t.toString();
}

        }//@members



/*
The grammar as defined in ~/svn/hats/WP1_Framework/Task-1.1_CoreABSLanguage/notes/abs.tex

P ::= _D_ _L_ {_T x_ ; sr}                     program
D ::= interface I {_Ms_}                        ifDecl
L ::= class C implements _I_ {_T f_; _M_}       clDecl
T ::= I | bool | fut(T)                         type
sr ::= s; return e                              stmtReturn
Ms ::= T m (_T x_)                              methSign
M ::= Ms{_T x_; sr}                             method

v ::= f | x                                     var
b ::= true | false                              bool
e ::= v | e.get | null | b                      expr
s ::= v = e | await g | skip | s; s |           stmt
            if e then s else s | release| 
            v = new C( ) | v = e!m(_e_) | 
            v = o.m(_e_) | v = m(_e_) 
g ::= v? | g && g                               guard

x                                               localVar 
f                                               field
m                                               methName 

Derived entities: 

xD ::= T x                                       varDecl
fD ::= T f                                       fieldDecl   


*** 
Does _X_ indicate X,X,X or X X X 

I make _X_ a Xlist and then define Xlist as X (COMMA! X*) then it is easier to change later. 
However D and L does not need commas, since they end with rbrace. 

  Ms does not need comma since it ends with rparen 

* I assume that XLists can always be empty (cf.igarashi.pierce.wadler_featherweight.pdf) : 

"We write f as shorthand for a possibly empty sequence f1, ...  ,fn (and
similarly for C, x, e, etc.) and write M as shorthand for M1 ... Mn (with no commas)."

In genral XList therefore is: 
  (X (COMMA! X)*)? 
or  just 
  X* 

*/

program
	:	
	ifDecl* clDecl* LBRACE varDeclList SEMI stmtRet RBRACE 
		-> ^(PROGRAM ifDecl* clDecl* varDeclList? ^(stmtRet))
    ;

ifDecl
	:	
	INTERFACE ifName LBRACE methSign* RBRACE
        -> ^(INTERFACE ifName (methSign)* ) 
	;


clDecl
    :
    CLASS className IMPLEMENTS ifNameList LBRACE fieldDeclList  SEMI method* RBRACE 
        -> ^(CLASS className ^(IMPLEMENTS ifNameList?)  ^(FIELDS fieldDeclList?)  ^(METHODS method*)) 
    ;





stmtRet : stmtList SEMI RETURN exPure
    -> ^(STMTS stmtList ^(RETURN exPure)) ;  


methSign 
    : 
    type methName LPAREN varDeclList RPAREN      
        -> ^(METHSIGN type methName ^(PARAMS varDeclList?)) 
        
    ;


ifNameList : (ifName (COMMA! ifName)*)? ;

varDeclList : (varDecl (COMMA! varDecl)*)? ;

fieldDeclList : (fieldDecl (COMMA! fieldDecl)*)? ;


method : methSign LBRACE varDeclList SEMI stmtRet RBRACE 
        -> ^(methSign ^(VARS varDeclList) ^(STMTS stmtRet)) 
    ;

varDecl : type localVar ;


fieldDecl : type field ; 


stmt :  stmtBlock 
    |   assignStmt  
    |   AWAIT guard 
    |   SKIP 
    |   IF exPure THEN stmt (ELSE stmt)? 
    |   RELEASE ; 


stmtBlock : LBRACE stmtList RBRACE
        //        -> stmtList 
    ;      


stmtList : (stmt (SEMI stmt)*) 
    ;

/* This is left recursive: 
expr : var | expr DOT GET | NULL | boolEx 
we redefine as: 
*/

exPure :  
        var exPureTail  |
        boolEx exPureTail |
        NULL exPureTail
    ; 

exPureTail :  (DOT GET)*  ;

exprList : (exPure (COMMA! exPure)*)? ;

type : 
        ifName -> ^(ifName)
    |   BOOL -> ^(BOOL)
    |   FUT LPAREN type RPAREN ->  ^(FUT type) 
    ;

assignStmt : 
        var ASSIGN exPure 
    |   var ASSIGN exEffect 
    ;

exEffect : 
      methName LPAREN exprList RPAREN 
    | exPure DOT methName LPAREN exprList RPAREN 
    | exPure BANG methName LPAREN exprList RPAREN
    | NEW className LPAREN RPAREN ; 


guard : var QMARK gTail ;

gTail : (AND guard)* ;


/* Left recursive 

guard : var QMARK | guard AND guard ; 

*/     

   
//var : field | localVar ; 

var : IDENT ; 


boolEx : TRUE | FALSE ; 


// ================
//     Names
// ================
	 
ifName      :   IDENT ; 
className   :   IDENT ; 

methName    :   IDENT ; 

objName     :   IDENT ; 

localVar    :   IDENT ; 
field       :   IDENT ; 


// ================
// Core ABS Lexer
// ================

INTERFACE	:	'interface';
CLASS	    :	'class';
IMPLEMENTS	:	'implements';
RETURN      :   'return';


LPAREN	    :	'(';
RPAREN      :	')';

LBRACE      :	'{';
RBRACE 	    :	'}';


BOOL	    : 	'bool';
FUT         :   'fut';

TRUE	    :	'true';
FALSE	    :	'false';

SKIP        :   'skip';
GET	        :	'get';
NULL        :	'null';

AWAIT	    :	'await';

IF	        :	'if';
THEN	    :	'then';
ELSE	    :	'else';
FI	        :	'fi';
RELEASE     :   'release'; 
NEW         :   'new'; 

ASSIGN 	    :	'=' ;
ASSIGNSTMT 	    :	'dummy' ;

SEMI	    :	';';
DOT	        :	'.' ;
COMMA	    :	',' ;
QMARK       :   '?' ; 
BANG        :   '!' ; 

AND	        :	'&&';


fragment DIGIT 
	:
		'0'..'9'	
	;

INTEGER 
    :
        DIGIT+	
	;

fragment LETTER 
	:	
        'a'..'z'
    |   'A'..'Z'
    |   '_'
    |   '$'
    |   '\\'
;

IDENT	
	:
	   LETTER ( LETTER | DIGIT )*
	;

WS  
    :   (    ' '
        |    '\r'
        |    '\t'
        |    '\u000C'
        |    '\n' ) { skip(); }          
    ;
   
COMMENT
    :   '/*'
        (options {greedy=false;} : . )* 
        '*/' { 
                 skip(); // if we want them accessible from the ADT better: $channel=HIDDEN;
             }
    ;

LINE_COMMENT
    :   '//' ~('\n'|'\r')*  ('\r\n' | '\r' | '\n') { skip(); }
    |   '//' ~('\n'|'\r')*  { skip();  }
    ;   


