/**
 * Copyright (c) 2014, Rudolf Schlatte. All rights reserved.
 * This file is licensed under the terms of the Modified BSD License.
 */

// TODO for moving tools from beaver/flex to antlr:
// - Generate code for full ABS
// - Implement the raiseExceptions flag

grammar ABS;

TraditionalComment : '/*' .*? '*/' -> skip ;
EndOfLineComment : '//' .*? ('\n' | EOF) -> skip ;
WhiteSpace : [ \t\f\r\n]+ -> skip ;

// Common lexical elements

fragment LETTER : [A-Za-z] ;
fragment DIGIT : [0-9] ;
fragment EXPONENT : ('e' | 'E' | 'e+' | 'E+' | 'e-' | 'E-') DIGIT+;
IDENTIFIER : [a-z] (LETTER | DIGIT | '_')* ;
TYPE_IDENTIFIER : 'Any' | ([A-Z] (LETTER | DIGIT | '_')*) ;
INTLITERAL : '0' | [1-9] DIGIT* ;
FLOATLITERAL : INTLITERAL? '.' DIGIT+ EXPONENT? ;
STRINGLITERAL
  :  '"' (STR_ESC | ~('\\' | '"' | '\r' | '\n'))* '"'
  ;
fragment STR_ESC
  :  '\\' ('\\' | '"' | 't' | 'n' | 'r')
  ;

TEMPLATESTRINGSTART
    : '`' TEMPLATESTRING_INNER* '$'
    ;

TEMPLATESTRINGINBETWEEN
    : '$' TEMPLATESTRING_INNER* '$'
    ;
TEMPLATESTRINGEND
    : '$' TEMPLATESTRING_INNER* '`'
    ;
TEMPLATESTRINGLITERAL
    : '`' TEMPLATESTRING_INNER* '`'
    ;

fragment TEMPLATESTRING_INNER
    : '\\' ('`' | '$')
    | ~('`' | '$')
    ;


NEGATION : '!' ;
MINUS : '-' ;
MULT : '*' ;
DIV : '/' ;
MOD : '%' ;
PLUS : '+' ;
LTEQ : '<=' ;
GTEQ : '>=' ;
LT : '<' ;
GT : '>' ;
ANDAND : '&&' ;
OROR : '||' ;
EQEQ : '==' ;
NOTEQ : '!=' ;
IMPLIES : '->' ;
EQUIV : '<->' ;

qualified_type_identifier : (TYPE_IDENTIFIER '.')* TYPE_IDENTIFIER ;
qualified_identifier : (TYPE_IDENTIFIER '.')* IDENTIFIER ;
// If this rule becomes more complicated, adapt exitAny_identifier() in
// CreateJastAddASTListener as well -- at the moment, we rely on there
// being exactly one token
any_identifier : qualified_type_identifier | qualified_identifier ;
// type_use has annotations
type_use : annotations n=qualified_type_identifier
        ('<' p+=type_use (',' p+=type_use)*  '>')? ;
// type_exp does not have annotations (on the base type)
// This is how we (seem to) disambiguate between annotations on
// methods/fields and their types.  Inherited from beaver grammar.
type_exp : n=qualified_type_identifier
        ('<' p+=type_use (',' p+=type_use)*  '>')? ;
paramlist : '(' (param_decl (',' param_decl)*)? ')' ;
param_decl : annotations type_exp IDENTIFIER ;

interface_name : qualified_type_identifier ;
delta_id : TYPE_IDENTIFIER ;

// Expressions

exp :         // Try eff_exp first - some of them have a pure_exp prefix
        eff_exp     # EffExp
    | pure_exp    # PureExp
    ;

eff_exp : pure_exp '.' 'get'                               # GetExp
    | 'new' l='local'? c=qualified_type_identifier
        '(' pure_exp_list ')'                             # NewExp
    | a='await'? o=pure_exp '!' m=IDENTIFIER
        '(' pure_exp_list ')'                             # AsyncCallExp
    | o=pure_exp '.' m=IDENTIFIER
        '(' pure_exp_list ')'                             # SyncCallExp
    | ((d=delta_id | c='core') '.')? 'original'
        '(' pure_exp_list ')'                             # OriginalCallExp
    ;

pure_exp : qualified_identifier '(' pure_exp_list ')'      # FunctionExp
    | qualified_identifier
        '(' function_list ')'
        '(' pure_exp_list ')'                              # PartialFunctionExp
    | qualified_identifier '[' pure_exp_list ']'           # VariadicFunctionExp
    | qualified_type_identifier ('(' pure_exp_list ')')?   # ConstructorExp
    | op=(NEGATION | MINUS) pure_exp                       # UnaryExp
    | l=pure_exp op=(MULT | DIV | MOD) r=pure_exp          # MultExp
    | l=pure_exp op=(PLUS | MINUS) r=pure_exp              # AddExp
    | l=pure_exp op=(LT | GT | LTEQ | GTEQ) r=pure_exp     # GreaterExp
    | l=pure_exp op=(EQEQ | NOTEQ) r=pure_exp              # EqualExp
    | l=pure_exp op='&&' r=pure_exp                        # AndExp
    | l=pure_exp op='||' r=pure_exp                        # OrExp
    | var_or_field_ref                                     # VarOrFieldExp
    | FLOATLITERAL                                         # FloatExp
    | INTLITERAL                                           # IntExp
    | STRINGLITERAL                                        # StringExp
    | TEMPLATESTRINGLITERAL                                # TemplateStringExp
    | TEMPLATESTRINGSTART e1=pure_exp
        (b+=TEMPLATESTRINGINBETWEEN e+=pure_exp)*
        TEMPLATESTRINGEND                                  # TemplateStringCompoundExp
    | 'this'                                               # ThisExp
    | 'null'                                               # NullExp
    | e=pure_exp 'implements' i=interface_name             # ImplementsExp
    | e=pure_exp 'as' i=interface_name                     # AsExp
    | 'if' c=pure_exp 'then' l=pure_exp 'else' r=pure_exp  # IfExp
    | 'case' c=pure_exp '{' casebranch+ '}'                # CaseExp
        // backward compatibility: do not demand (parentheses) around
        // the variable declaration but silently accept them for now.
    | 'let' '('? t+=type_use id+=IDENTIFIER ')'? '=' e+=pure_exp
        (',' '('? t+=type_use id+=IDENTIFIER ')'? '=' e+=pure_exp)*
        'in' body=pure_exp                                 # LetExp
    | '(' pure_exp ')'                                     # ParenExp
    ;

casebranch : pattern '=>' pure_exp ';' ;

pattern : '_'                                              # UnderscorePattern
    | INTLITERAL                                           # IntPattern
    | STRINGLITERAL                                        # StringPattern
    | IDENTIFIER                                           # VarPattern
    | qualified_type_identifier
        ('(' (pattern (',' pattern)*)? ')')?               # ConstructorPattern
    ;

var_or_field_ref : ('this' '.')? IDENTIFIER ;

pure_exp_list : (pure_exp (',' pure_exp)*)? ;

list_literal : '[' pure_exp (',' pure_exp)* ']' ;

// Annotations
annotation : (l=type_use ':')? r=pure_exp ;

annotations: ('[' al+=annotation (',' al+=annotation)* ']')* ;

// Statements

stmt : annotations type_exp IDENTIFIER ('=' exp)? ';'              # VardeclStmt
    | annotations var_or_field_ref '=' exp ';'                     # AssignStmt
    | annotations 'skip' ';'                                       # SkipStmt
    | annotations 'return' exp ';'                                 # ReturnStmt
    | annotations 'assert' exp ';'                                 # AssertStmt
    | annotations '{' stmt* '}'                                    # BlockStmt
    | annotations 'if' '(' c=pure_exp ')' l=stmt ('else' r=stmt)?  # IfStmt
    | annotations 'while' '(' c=pure_exp ')' stmt                  # WhileStmt
    | annotations 'foreach' '(' i=IDENTIFIER 'in' l=pure_exp ')' stmt
                                                                   # ForeachStmt
    | annotations 'try' b=stmt
        'catch' (('{' casestmtbranch* '}') | casestmtbranch)
        ('finally' f=stmt)?                                        # TryCatchFinallyStmt
    | annotations 'await' guard ';'                                # AwaitStmt
    | annotations 'suspend' ';'                                    # SuspendStmt
    | annotations 'duration' '(' f=pure_exp ',' t=pure_exp ')' ';' # DurationStmt
    | annotations 'throw' pure_exp ';'                             # ThrowStmt
    | annotations 'die' pure_exp ';'                               # DieStmt
    | annotations 'movecogto' pure_exp ';'                         # MoveCogToStmt
    | annotations exp ';'                                          # ExpStmt
        // Prefer case expression to case statement, so case statement comes later
    | annotations 'case' c=pure_exp '{' casestmtbranch* '}'        # CaseStmt
        // case
    ;

guard : var_or_field_ref '?'                           # ClaimGuard
    | 'duration' '(' min=pure_exp ',' max=pure_exp ')' # DurationGuard
    | e=pure_exp                                       # ExpGuard
    | l=guard '&' r=guard                              # AndGuard
    ;                           // TODO: objectguard

casestmtbranch : pattern '=>' stmt ;


// Datatypes

datatype_decl :
        annotations
        'data' n=qualified_type_identifier
        ('<' p+=TYPE_IDENTIFIER (',' p+=TYPE_IDENTIFIER)*  '>')?
        ('=' c+=data_constructor ('|' c+=data_constructor)*)? ';' ;

data_constructor : n=TYPE_IDENTIFIER
        ('(' a+=data_constructor_arg (',' a+=data_constructor_arg)* ')')? ;

data_constructor_arg : type_use IDENTIFIER? ;

// Type synonyms

typesyn_decl : annotations
        'type' qualified_type_identifier '=' type_use ';' ;

// Exceptions

exception_decl : annotations
        'exception' n=qualified_type_identifier
        ('(' a+=data_constructor_arg (',' a+=data_constructor_arg)* ')')? ';' ;

// Functions

function_decl : annotations
        'def' type_use n=qualified_identifier
        ('<' p+=TYPE_IDENTIFIER (',' p+=TYPE_IDENTIFIER)*  '>')?
        paramlist
        '='
        ('builtin' ('[' (builtin_args+=STRINGLITERAL (',' builtin_args+=STRINGLITERAL)* )? ']')?
        | e=pure_exp )
        ';'
    ;

// Partially defined functions

function_name_decl: IDENTIFIER ;
function_name_list: (function_name_decl (',' function_name_decl)*)? ;
type_use_paramlist: ('<' p+=type_use (',' p+=type_use)* '>') ;

par_function_decl : annotation*
        'def' type_use n=qualified_identifier
        ('<' p+=TYPE_IDENTIFIER (',' p+=TYPE_IDENTIFIER)*  '>')?
        '(' functions=function_name_list ')'
        params=paramlist
        '='
        e=pure_exp ';' ;

// used by PartialFunctionExp
function_name_param_decl: IDENTIFIER ;
function_param: function_name_param_decl | anon_function_decl ;
function_list: function_param (',' function_param)* ;

// Anonymous functions

anon_function_decl : params=paramlist '=>' pure_exp ;

// Interfaces

interface_decl : annotations
        'interface' qualified_type_identifier
        ('extends' e+=interface_name (',' e+=interface_name)*)?
        '{' methodsig* '}'
    ;

methodsig : annotations type_use IDENTIFIER paramlist ';' ;

// Classes

class_decl : annotations
        'class' qualified_type_identifier paramlist?
        ('implements' interface_name (',' interface_name)*)?
        '{'
        field_decl*
        ('{' stmt* '}')?
        ( 'recover' '{' casestmtbranch* '}' )?
        trait_usage*
        method*
        '}'
    ;

field_decl : annotations type_use IDENTIFIER ('=' pure_exp)? ';' ;

method : annotations type_use IDENTIFIER paramlist '{' stmt* '}' ;

// Module declaration
module_decl : 'module' qualified_type_identifier ';'
        ( exports += module_export | imports += module_import )*
        decl*
        main_block? ;

module_export : 'export'
        ( '*' | s+=any_identifier (',' s+=any_identifier)*)
        ('from' f=qualified_type_identifier)?
        ';'
    ;

module_import : 'import'
        ( '*' 'from' f=qualified_type_identifier ';'
        | s+=any_identifier (',' s+=any_identifier)* 'from' f=qualified_type_identifier ';'
        | s+=any_identifier (',' s+=any_identifier)* ';'
        )
    ;

// Top-level declarations

// If this rule becomes more complicated, adapt exitDecl() in
// CreateJastAddASTListener as well -- at the moment, we rely on there being
// exactly one token
decl : datatype_decl
    | function_decl
    | par_function_decl
    | typesyn_decl
    | exception_decl
    | interface_decl
    | class_decl
    | trait_decl
    ;


trait_decl : annotations
        'trait' qualified_type_identifier '=' trait_expr ;

trait_expr : basic_trait_expr trait_oper* ;

basic_trait_expr : '{' method* '}'      #TraitSetFragment
    | method                            #TraitSetFragment
    | TYPE_IDENTIFIER                   #TraitNameFragment
    ;

trait_oper : 'removes' methodsig                    #TraitRemoveFragment
    | 'removes' '{' methodsig* '}'                  #TraitRemoveFragment
    | 'adds' basic_trait_expr                       #TraitAddFragment
    | 'modifies' basic_trait_expr                   #TraitModifyFragment
    ;

trait_usage: 'uses' trait_expr ';' ;

delta_decl : 'delta' TYPE_IDENTIFIER
        ('(' p+=delta_param (',' p+=delta_param)* ')')? ';'
        delta_used_module?
        module_modifier*
        ;

delta_param : param_decl                            # DeltaFieldParam
    | (qualified_type_identifier has_condition)     # DeltaClassParam
    ;

has_condition : 'hasField' f=field_decl             # DeltaHasFieldCondition
    | 'hasMethod' m=methodsig                       # DeltaHasMethodCondition
    | 'hasInterface' i=interface_name               # DeltaHasInterfaceCondition
    ;

delta_used_module : 'uses' qualified_type_identifier ';' ;

// If this rule becomes more complicated, adapt exitModule_modifier() in
// CreateJastAddASTListener as well -- at the moment, we rely on there being
// exactly one token
module_modifier : functional_modifier | oo_modifier | namespace_modifier ;

functional_modifier : 'adds' function_decl      # DeltaAddFunctionModifier
    | 'adds' datatype_decl                      # DeltaAddDataTypeModifier
    | 'adds' typesyn_decl                       # DeltaAddTypeSynModifier
    | 'modifies' typesyn_decl                   # DeltaModifyTypeSynModifier
    | 'modifies' datatype_decl                  # DeltaModifyDataTypeModifier
    ;

oo_modifier : 'adds' class_decl                            # DeltaAddClassModifier
    | 'removes' 'class' qualified_type_identifier ';'      # DeltaRemoveClassModifier
    | 'modifies' 'class' n=qualified_type_identifier
        ('adds' ia+=interface_name (',' ia+=interface_name)*)?
        ('removes' ir+=interface_name (',' ir+=interface_name)*)?
        '{' class_modifier_fragment*? '}'                  # DeltaModifyClassModifier
    | 'adds' interface_decl                                # DeltaAddInterfaceModifier
    | 'removes' 'interface' qualified_type_identifier ';'  # DeltaRemoveInterfaceModifier
    | 'modifies' 'interface' qualified_type_identifier
        '{' interface_modifier_fragment* '}'               # DeltaModifyInterfaceModifier
    ;

class_modifier_fragment : 'adds' field_decl  # DeltaAddFieldFragment
    | 'removes' field_decl                   # DeltaRemoveFieldFragment
    | trait_oper                             # DeltaTraitFragment
    ;

interface_modifier_fragment : 'adds' methodsig   # DeltaAddMethodsigFragment
    | 'removes' methodsig                       # DeltaRemoveMethodsigFragment
    ;

namespace_modifier : 'adds' module_import    # DeltaAddModuleImportFragment
    | 'adds' module_export                   # DeltaAddModuleExportFragment
    ;

// Product line
productline_decl : 'productline' TYPE_IDENTIFIER ';'
        'features' feature (',' feature)* ';'
        delta_clause* ;

feature : TYPE_IDENTIFIER (p='\'')? // allow writing feat' to refer to the
                                    // intermediate feature "$feat"
        ('{' attr_assignment (',' attr_assignment)* '}')? ;

delta_clause : 'delta' deltaspec after_condition? from_condition? when_condition? ';' ;

// FCL
// A delta specification is a name + an optional list of feature attributes or
// "has" parameters
// FIXME from ABS.parser: support "hasMethod"/"hasAttribute" parameters
deltaspec : TYPE_IDENTIFIER ('(' deltaspec_param (',' deltaspec_param)* ')')? ;

// the following todo copied from ABS.parser:
// TODO: accept feature, attributes, or constants (DataExp).
// CID | FID.aid | CONSTANT
deltaspec_param : TYPE_IDENTIFIER '.' IDENTIFIER # FIDAIDDeltaspecParam
    | INTLITERAL                                 # IntDeltaspecParam
    | TYPE_IDENTIFIER                            # BoolOrIDDeltaspecParam
    ;

after_condition : 'after' delta_id (',' delta_id)* ;

from_condition : 'from' application_condition ;

when_condition : ('when' | 'to') application_condition ;

application_condition
    : NEGATION application_condition                          # NotApplicationCondition
    | l=application_condition ANDAND r=application_condition  # AndApplicationCondition
    | l=application_condition OROR r=application_condition    # OrApplicationCondition
    | '(' application_condition ')'                           # ParenApplicationCondition
    | feature                                                 # FeatureApplicationCondition
    ;

// FIXME: Only integers or True/False are allowed here - introduce 'True' and
// 'False' keywords and treat them in DataConstructor as well?  Currently we
// see all non-'True' as 'False', which is wrong.
attr_assignment : IDENTIFIER '=' (i=INTLITERAL | b=TYPE_IDENTIFIER | s=STRINGLITERAL) ;

// Products
product_decl : 'product' TYPE_IDENTIFIER
    (
      '(' (feature (',' feature)*)? ')' ';'
    |
      '=' product_expr ';'
    )
    ;

// Product Expression
product_expr
    : '{' feature (',' feature)* '}'                          # ProductFeatureSet
    | l=product_expr ANDAND r=product_expr                    # ProductIntersect
    | l=product_expr OROR r=product_expr                      # ProductUnion
    | l=product_expr MINUS r=product_expr                     # ProductDifference
    | TYPE_IDENTIFIER                                         # ProductName
    | '(' product_expr ')'                                    # ProductParen
    ;

// mTVL Feature model
fextension : TYPE_IDENTIFIER '{'
        feature_decl_group? (feature_decl_attribute | feature_decl_constraint)* '}' ;

feature_decl : TYPE_IDENTIFIER
        ('{' feature_decl_group?
            (feature_decl_attribute | feature_decl_constraint)* '}')?
    ;

feature_decl_group : 'group'
        (o='oneof' | a='allof' | '[' l=INTLITERAL '..' (u=INTLITERAL | s='*') ']')
        '{' fnode (',' fnode)* '}'
    ;

fnode : o='opt'? feature_decl
    ;

feature_decl_attribute :
        TYPE_IDENTIFIER IDENTIFIER 'in' '{' is+=boundary_val (',' is+=boundary_val)* '}' ';'
    | TYPE_IDENTIFIER IDENTIFIER 'in' '[' l=boundary_int '..' u=boundary_int ']' ';'
    | TYPE_IDENTIFIER '[' l=boundary_int '..' u=boundary_int ']' IDENTIFIER ';'
    | TYPE_IDENTIFIER IDENTIFIER ';'
;

feature_decl_constraint :
      ('ifin' ':')? mexp ';'               # FeatureDeclConstraintIfIn
    | 'ifout' ':' mexp ';'                 # FeatureDeclConstraintIfOut
    | 'exclude' ':' TYPE_IDENTIFIER ';'    # FeatureDeclConstraintExclude
    | 'require' ':' TYPE_IDENTIFIER ';'    # FeatureDeclConstraintRequire
    ;

mexp : TYPE_IDENTIFIER '.' IDENTIFIER
    | TYPE_IDENTIFIER
    | INTLITERAL
    | IDENTIFIER
    | op=(NEGATION | MINUS) a=mexp
    | l=mexp op=(MULT | DIV | MOD) r=mexp
    | l=mexp op=(PLUS | MINUS) r=mexp
    | l=mexp op=(LT | GT | LTEQ | GTEQ) r=mexp
    | l=mexp op=(EQEQ | NOTEQ) r=mexp
    | l=mexp op=(IMPLIES | EQUIV) r=mexp
    | l=mexp op=ANDAND r=mexp
    | l=mexp op=OROR r=mexp
    | '(' a=mexp ')'
    ;

boundary_int : star='*' | boundary_val ;

boundary_val : m='-'? i=INTLITERAL ;

main_block : annotations '{' stmt* '}' ;

compilation_unit : module_decl* delta_decl*
        productline_decl? product_decl*
        ('root' feature_decl | 'extension' fextension)*
    ;

goal : compilation_unit EOF ;

