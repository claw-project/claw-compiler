/**
 * ANTLR 4 Grammar file for the Fortran dependencies parser. It only covers individual statements 
 * relevant to the extraction of dependencies information.
 *
 * @author Mikhail Zhigun
 * @copyright 2020, MeteoSwiss
 */
grammar FortranProcedureStatementsRecognizer;

function_open_stmt : subroutine_specifier*  return_type? subroutine_specifier* FUNCTION function_open_name
                     PROCEDURE_ARGS (FUNCTION_RESULT | PROCEDURE_BIND)* EOF;
function_open_name : identifier;

function_close_stmt : ((END (FUNCTION function_close_name?)?) | (ENDFUNCTION function_close_name?)) EOF;
function_close_name : identifier;

subroutine_open_stmt : subroutine_specifier* SUBROUTINE subroutine_open_name (PROCEDURE_ARGS)? PROCEDURE_BIND? EOF;
subroutine_open_name : identifier;

subroutine_close_stmt : ((END (SUBROUTINE subroutine_close_name?)?) | (ENDSUBROUTINE subroutine_close_name?)) EOF;
subroutine_close_name : identifier;

// Lexer cannot differentiate between ambiguous keywords and identifiers
identifier : IDENTIFIER |
DOUBLE |
ELEMENTAL |
ENDFUNCTION | ENDPROGRAM | ENDSUBROUTINE |
END |
FUNCTION  |
PRECISION |
PURE  |
RECURSIVE  |
RESULT  |
SUBROUTINE |
TYPE | BASIC_TYPE_ID;

subroutine_specifier : PURE | ELEMENTAL | RECURSIVE;

return_type : USER_TYPE | (BASIC_TYPE_ID | DOUBLE_PRECISION) | BASIC_TYPE_WITH_SPEC;
USER_TYPE : TYPE SEP? '(' SEP? IDENTIFIER SEP? ')';
BASIC_TYPE_WITH_SPEC : (BASIC_TYPE_ID | DOUBLE_PRECISION) SEP? EXPR;
fragment EXPR : '(' (  EXPR (NON_EXPR EXPR)* (NON_EXPR)? | NON_EXPR (EXPR NON_EXPR)* (EXPR)?) ')';
fragment NON_EXPR : (~[\n\r;()])+;
FUNCTION_RESULT : RESULT SEP? '(' SEP? IDENTIFIER SEP? ')';
PROCEDURE_BIND : BIND SEP? '(' SEP? C SEP? (',' SEP? NAME SEP? '=' SEP? STRING SEP?)? ')';

fragment BIND : B I N D;
fragment NAME : N A M E;
fragment STRING : ((DQ (~'"' | QUOTED_DQ)* DQ) |
          (SQ (~'\'' | QUOTED_SQ)* SQ));
fragment QUOTED_DQ : DQ DQ;
fragment QUOTED_SQ : SQ SQ;
fragment DQ : '"';
fragment SQ : '\'';

PROCEDURE_ARGS : '(' ((PROCEDURE_ARG (',' PROCEDURE_ARG)*) | SEP)? ')';
fragment PROCEDURE_ARG : SEP? IDENTIFIER SEP?;

BASIC_TYPE_ID : INTEGER | REAL | DOUBLEPRECISION | COMPLEX | CHARACTER | LOGICAL;

DOUBLE_PRECISION : DOUBLE SEP PRECISION;

fragment CHARACTER : C H A R A C T E R;
fragment COMPLEX : C O M P L E X;
fragment DOUBLEPRECISION : D O U B L E P R E C I S I O N;
fragment LOGICAL : L O G I C A L;
fragment INTEGER : I N T E G E R;
fragment REAL : R E A L;

DOUBLE : D O U B L E;
PRECISION : P R E C I S I O N;
ELEMENTAL: E L E M E N T A L;
ENDFUNCTION : E N D F U N C T I O N;
ENDPROGRAM : E N D P R O G R A M;
ENDSUBROUTINE : E N D S U B R O U T I N E;
END : E N D;
FUNCTION : F U N C T I O N;
PURE : P U R E;
RECURSIVE : R E C U R S I V E;
RESULT : R E S U L T;
SUBROUTINE : S U B R O U T I N E;
TYPE : T Y P E;

IDENTIFIER : LETTER (LETTER | DIGIT | '_')*;

fragment LETTER : [a-zA-Z];
fragment DIGIT : [0-9];

SEP : WS+ -> skip;
fragment WS : [ \t];

fragment A : [aA];
fragment B : [bB];
fragment C : [cC];
fragment D : [dD];
fragment E : [eE];
fragment F : [fF];
fragment G : [gG];
fragment H : [hH];
fragment I : [iI];
fragment J : [jJ];
fragment K : [kK];
fragment L : [lL];
fragment M : [mM];
fragment N : [nN];
fragment O : [oO];
fragment P : [pP];
fragment Q : [qQ];
fragment R : [rR];
fragment S : [sS];
fragment T : [tT];
fragment U : [uU];
fragment V : [vV];
fragment W : [wW];
fragment X : [xX];
fragment Y : [yY];
fragment Z : [zZ];
