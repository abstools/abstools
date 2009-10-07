grammar ABS;

options {
	output = AST;
	ASTLabelType=CommonTree;
}

tokens {
	PROGRAM;
	METHODDECLARATION;
	FUNCTIONDECLARATION;
	VARIABLEDECLARATION;
	CONSTRUCTORDECLARATION;
	
	EXTFIELDS;
	INTFIELDS;
	FIELDSPECIFICATION;

	MEMBERREFERENCE;
	VARIABLEREFERENCE;
	TYPEREFERENCE;
	
	FUNCTIONEVAL;
}

@parser::header { 
package parser;

}

@lexer::header { 
package parser;
}

@members {
public static void main(String[] args) throws Exception {
	ABSLexer lex = new ABSLexer(new ANTLRFileStream(args[0]));
	CommonTokenStream tokens = new CommonTokenStream(lex);

	ABSParser parser = new ABSParser(tokens);

	try {
    	parser.program();
	} catch (RecognitionException e)  {
		e.printStackTrace();
	}
}
}

program
	:	
	declaration* expression 
		-> ^(PROGRAM declaration* expression)
	;

// ================
//   Declarations
// ================
	
declaration 
	:	
	interfaceDeclaration        | 
	classDeclaration            | 
	abstractDataTypeDeclaration | 
	functionDeclaration		
	;
	
interfaceDeclaration
	:	
	INTERFACE typeName (EXTENDS typeNameList)? LBRACE RBRACE
  	                        -> ^(INTERFACE typeName (EXTENDS typeNameList)? ) 
	                       
	;
	
foo : typeName  (COMMA! typeName)* ;

// FOOEXT : 'suppe';

classDeclaration
	:
	CLASS typeName (LPAREN parameterList RPAREN)? (IMPLEMENTS typeNameList)? 	
	LBRACE classBody RBRACE 
        -> ^(CLASS typeName ^(IMPLEMENTS typeNameList) ^(EXTFIELDS parameterList)? classBody)
	;
    // TODO: -> .. ^(IMPLEMENTS typeNameList)?

typeNameList
	:
	typeName (COMMA! typeName)* 	                     
	;

classBody
	:
	((variableOrFieldDeclaration ASSIGN expression SEMI)* methodDeclaration*)
		-> ^(FIELDSPECIFICATION variableOrFieldDeclaration expression)* methodDeclaration*
	;
		
methodDeclaration
	:
	functionMethodName LPAREN parameterList RPAREN 
	LBRACE
	  // methodBody to come
	RBRACE -> ^(METHODDECLARATION functionMethodName parameterList)
	;		
		
functionDeclaration
	:	
	DEF functionMethodName LPAREN parameterList RPAREN
		-> ^(FUNCTIONDECLARATION functionMethodName parameterList)
	;
	
abstractDataTypeDeclaration
	:
	DATA typeName ASSIGN constructor (OR constructor)*
	-> ^(DATA typeName constructor+)
	;
	
constructor
	:
	functionMethodName LPAREN typeReference RPAREN
		->^(CONSTRUCTORDECLARATION functionMethodName typeReference)
	;

variableOrFieldDeclaration
	:
	(locationName COLON typeReference) -> ^(VARIABLEDECLARATION locationName typeReference)
	;	

parameterList 
	:	
	(variableOrFieldDeclaration (COMMA! variableOrFieldDeclaration)*)?
	;

// ================
// EXPRESSIONS
// ================

expression
	:
		selectorExpression (ASSIGN^ expression)?
	;		

selectorExpression
	:
	(primaryExpression -> primaryExpression)
	  (selector -> ^(MEMBERREFERENCE primaryExpression selector)) ?
	;
	
selector 
	:
	 DOT! (suspendExpressionSuffix | methodReferenceSuffix | locationName)
	; 
		
primaryExpression
	:	
	THIS                   | 
	localVariableReference | 	
	functionOrConstructorEvaluation |
	creationExpression     |
	caseExpression         |
	YIELD |
	INTEGER
	;
	
caseExpression
	:
	CASE expression OF
	  branch (OR branch)*
	END -> ^(CASE expression branch+)
	;	

branch 	:	
	guard=(pattern | ELSE) ARROW expression -> ^(ARROW $guard expression) 
	;
	
pattern :	localVariableReference | 
		UNDERSCORE | 
		functionMethodName LPAREN (pattern (COMMA pattern)*)? RPAREN -> ^(FUNCTIONEVAL functionMethodName pattern*)
	// TODO: -> ... (pattern*)?  OR...?	
	;
	
// side effect free expressions and initialising expression should be checked in a second pass
// and not encode in the grammar, otherwise consistency problems with the expression rule
	
localVariableReference 
	:
	locationName -> ^(VARIABLEREFERENCE locationName)
	;
		
methodReferenceSuffix
	:
	functionMethodName LPAREN argumentList RPAREN 
	;

suspendExpressionSuffix
	:	
	    AWAIT | GET 
	;

functionOrConstructorEvaluation
	:
	functionMethodName LPAREN argumentList RPAREN 
	 -> ^(FUNCTIONEVAL functionMethodName argumentList)
	;

argumentList
	:
	(expression (COMMA! expression)*)?
	;	

creationExpression
	:
	NEW^ typeName LPAREN! argumentList RPAREN!
	;

typeReference
	:
	typeName LT (typeReference (COMMA typeReference)*)? GT | 
	typeName |
	BOOL | UNIT | MAYBE 
	// | tv ??
	;
	
// ================
//     Names
// ================
	 
// for the moment these rules could be inlined, but maybe we want 
// to allow different character sequence for certain names
typeName
	:	 IDENT
	;

functionMethodName 
	:	 IDENT
	;

locationName 
	:	 IDENT
	;


// ================
// Core ABS Lexer
// ================

INTERFACE
	:	'interface';
CLASS	:	'class';
DATA	:	'data';
EXTENDS	:	'extends';
IMPLEMENTS 
	:	'implements';
DEF	:	'DEF';

BOOL	: 	'Bool';
UNIT	:	'Unit';
MAYBE	:	'Maybe';

JUST	:	'Just';
NOTHING	:	'Nothing';

TRUE	:	'True';
FALSE	:	'False';

AWAIT	:	'await';
CASE	: 	'case';
ELSE	:	'else';
END	:	'end';
GET	:	'get';
NEW	:	'new';
OF	:	'of';
YIELD  	:	'yield';


DOT	:	'.';
COLON	:	':';
COMMA	:	',';
SEMI	:	';';
ARROW 	:	'->';

THIS	:	'this';
ASSIGN 	:	':=';
EQUALS 	:	'=';

LPAREN	:	'(';
RPAREN  :	')';

LBRACE  :	'{';
RBRACE 	:	'}';

LBRACKET:	'[';
RBRACKET:	']';

PLUS	:	'+';
MINUS	:	'-';
TIMES	:	'*';
DIV	:	'/';
MOD	:	'%';
EXP	:	'^';

LT	:	'<';
LEQ	:	'<=';
GT	:	'>';
GEQ	:	'>=';

AND	:	'&';
OR	:	'|';
NOT	:	'!';
IMPLIES	:	'=>';

UNDERSCORE 
	:	'_';

fragment DIGIT 
	:
		'0'..'9'	
	;

INTEGER :
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
