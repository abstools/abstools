//$Id$

grammar ABS;

options {
	output = AST;
	ASTLabelType=CommonTree;
}

tokens {
	PROGRAM;
    FIELDDECL;
    INTERFACE ;
	METHODDECL;
	FUNCTIONDECL;
	VARDECL;
	CONSTRUCTORDECL;
    METHSIGN ;
	EXTFIELDS;
	INTFIELDS;
	FIELDSPECIFICATION;
    PARAMS ; 
	MEMBERREFERENCE;
	VARIABLEREFERENCE;
	TYPEREFERENCE;
	
	FUNCTIONEVAL;
}


@parser::header { 
package abs.frontend.parser;
}

@lexer::header { 
package abs.frontend.parser;
}

@members {
public static void main(String[] args) throws Exception {

    boolean treeflag = true;
	ABSLexer lex = new ABSLexer(new ANTLRFileStream(args[0]));
	CommonTokenStream tokens = new CommonTokenStream(lex);
	ABSParser parser = new ABSParser(tokens);

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


}

/*
The grammar as define in ~/svn/hats/WP1_Framework/Task-1.1_CoreABSLanguage/notes/abs.tex
¯
P ::= ¯D¯ ¯L¯ {¯(T x)¯; sr}                     program
D ::= interface I {¯Ms¯}                        ifDecl
sr ::= s; return e                              stmtReturn
L ::= class C implements ¯I¯ {¯T f¯; ¯M¯}       clDecl
v ::= f | x                                     var
M ::= Ms{¯T x¯; sr}                             method
b ::= true | false                              bool
e ::= v | e.get | null | b                      expr
T ::= I | bool | fut(T)                         type
s ::= v = e | await g | skip | s; s |           stmt
            if e then s else s | release| 
            v = new C( ) | v = e!m(¯e¯) | 
            v = o.m(¯e¯) | v = m(¯e¯) 

Ms ::= T m (¯T x¯)                              methSign

g ::= v? | g && g                               guard

x                                               localVar 
f                                               field
m                                               methName 

Derived entities: 

xD ::= T x                                       varDecl
fD ::= T f                                       fieldDecl   


 

*** 
Does ¯X¯ indicate X,X,X or X X X 

I make ¯X¯ a Xlist and then define Xlist as X (COMMA! X*) then it is easier to change later. 
However ¯D¯ and ¯L¯ does not need commas. Since they end with rbrace. 
  ¯Ms¯ does not need comma since it ends with rparen 

* I assume that XLists can always be empty (cf.igarashi.pierce.wadler_featherweight.pdf) : 

"We write ¯f¯ as shorthand for a possibly empty sequence f1, ...  ,fn (and
similarly for ¯C¯, ¯x¯, ¯e¯, etc.) and write ¯M¯ as shorthand for M1 ... Mn (with no commas)."

* thus XList is: (X (COMMA! X)*)? 
* or  just X* 

Deviations : 
- I use && for ^
- I use "if then else _fi_" 


questions: 

the following is allowed by the grammar as it is now, is that correct:

class foo implements { ; } i.e. ¯I¯  is empty 

this is _not_ allowed in the current grammar: 

class foo  { ; } i.e. "implements" is left out





*/


program
	:	
	ifDecl* clDecl* LBRACE varDeclList SEMI stmtRet RBRACE 
		-> ^(PROGRAM ifDecl* clDecl* varDeclList? stmtRet)
    ;

ifDecl
	:	
	INTERFACE ifName LBRACE methSign* RBRACE
        -> ^(INTERFACE ifName (methSign)* ) 
	;


clDecl
    :
    CLASS className IMPLEMENTS ifNameList LBRACE fieldDeclList  SEMI method* RBRACE 
        -> ^(CLASS className ^(IMPLEMENTS ifNameList?)  ^(FIELDDECL fieldDeclList?)  ^(METHODDECL method*)) 
    ;



varDeclList : (varDecl (COMMA! varDecl)*)? ;
    


stmtRet : stmList SEMI RETURN expr ;  


methSign 
    : 
    type methName LPAREN varDeclList RPAREN      
        -> ^(METHSIGN type methName ^(PARAMS varDeclList?)) 
        
    ;


ifNameList : (ifName (COMMA! ifName)*)? ;


fieldDeclList : (fieldDecl (COMMA! fieldDecl)*)? ;


method : methSign LBRACE varDeclList SEMI stmtRet RBRACE ;

varDecl : type localVar 
    -> ^(VARDECL type localVar) ; 

fieldDecl: type field ; 


/*
stmt : 
        assignStmt | 
        AWAIT guard |
        SKIP |
        stmt SEMI stmt |
        IF expr THEN stmt ELSE stmt |
        RELEASE 
    ;
*/

stmt :  assignStmt |
        AWAIT guard |
        SKIP |
        IF expr THEN stmList ELSE stmList FI |
        RELEASE
    ; 

stmList : (stmt (SEMI! stmt)*) ;

/* This is left recursive: 
expr : var | expr DOT GET | NULL | boolEx 

rewrites to the following: 

*/
        
expr :  
        var expTail |
        boolEx expTail |
        NULL expTail
    ; 

expTail :  
        DOT GET |
    ;


type : 
        ifName |
        BOOL |
        FUT LPAREN type RPAREN
    ;

assignStmt : 
        var ASSIGN expr |
        var ASSIGN NEW className LPAREN RPAREN |
        var ASSIGN expr BANG methName LPAREN expr* RPAREN | 
        var ASSIGN objName DOT methName LPAREN expr* RPAREN | 
        var ASSIGN methName LPAREN expr* RPAREN 
    ;


guard : var QMARK gTail ;

gTail : AND guard | ;

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


