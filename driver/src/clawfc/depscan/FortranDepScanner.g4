/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 * @author Mikhail Zhigun
*/
/**
 * ANTLR 4 Grammar file for the Fortran File Info scanner. Extracts information about program units and dependencies.
 */
grammar FortranDepScanner;

root : (module_open_stmt | module_close_stmt | program_open_stmt | program_close_stmt | function_open_stmt |
function_close_stmt | subroutine_open_stmt | subroutine_close_stmt | block_data_open_stmt | block_data_close_stmt |
program_unit_close_stmt | use_stmt)* EOF;

module_open_stmt : MODULE_OPEN_STMT;
module_close_stmt: MODULE_CLOSE_STMT;
program_open_stmt : PROGRAM_OPEN_STMT;
program_close_stmt : PROGRAM_CLOSE_STMT;
function_open_stmt : FUNCTION_OPEN_STMT; 
function_close_stmt: FUNCTION_CLOSE_STMT;
subroutine_open_stmt : SUBROUTINE_OPEN_STMT; 
subroutine_close_stmt: SUBROUTINE_CLOSE_STMT;
block_data_open_stmt : BLOCK_DATA_OPEN_STMT; 
block_data_close_stmt: BLOCK_DATA_CLOSE_STMT;
program_unit_close_stmt: PROGRAM_UNIT_CLOSE_STMT;
use_stmt : USE_STMT;

MODULE_OPEN_STMT : SEP? MODULE SEP IDENTIFIER SEP?;
PROGRAM_OPEN_STMT : SEP? PROGRAM SEP IDENTIFIER SEP?;
FUNCTION_OPEN_STMT : SEP? (SUBROUTINE_SPECIFIER SEP)* RETURN_TYPE? (SUBROUTINE_SPECIFIER SEP)* FUNCTION SEP IDENTIFIER SEP? PROCEDURE_ARGS SEP?
                     ((FUNCTION_RESULT SEP?) | (PROCEDURE_BIND SEP?))*;
SUBROUTINE_OPEN_STMT : SEP? (SUBROUTINE_SPECIFIER SEP)* SUBROUTINE SEP IDENTIFIER SEP? (PROCEDURE_ARGS SEP?)? (PROCEDURE_BIND SEP?)?;
BLOCK_DATA_OPEN_STMT : SEP? BLOCK SEP? DATA (SEP IDENTIFIER)? SEP?;

USE_STMT : SEP? USE SEP IDENTIFIER SEP? (',' SEP? (RENAME_LIST_STMT | ONLY_LIST_STMT))?;

MODULE_CLOSE_STMT : SEP? END SEP? MODULE (SEP IDENTIFIER)? SEP?;
PROGRAM_CLOSE_STMT : SEP? END SEP? PROGRAM (SEP IDENTIFIER)? SEP?;
SUBROUTINE_CLOSE_STMT : SEP? END SEP? SUBROUTINE (SEP IDENTIFIER)? SEP?;
FUNCTION_CLOSE_STMT : SEP? END SEP? FUNCTION (SEP IDENTIFIER)? SEP?;
BLOCK_DATA_CLOSE_STMT : SEP? END SEP? BLOCK SEP? DATA (SEP IDENTIFIER)? SEP?;
PROGRAM_UNIT_CLOSE_STMT : SEP? END SEP?;

OTHER : (~[\r\n;'"] | STRING)+ ->skip;

fragment SUBROUTINE_SPECIFIER : PURE | ELEMENTAL | RECURSIVE;

fragment RETURN_TYPE : USER_TYPE SEP? | (BASIC_TYPE_ID  | DOUBLE_PRECISION) SEP | BASIC_TYPE_WITH_SPEC SEP?;
fragment USER_TYPE : TYPE SEP? '(' SEP? IDENTIFIER SEP? ')';
fragment BASIC_TYPE_WITH_SPEC : (BASIC_TYPE_ID | DOUBLE_PRECISION) SEP? EXPR;
fragment EXPR : '(' (  EXPR (NON_EXPR EXPR)* (NON_EXPR)? | NON_EXPR (EXPR NON_EXPR)* (EXPR)?) ')';
fragment NON_EXPR : (~[\n\r;()])+;
fragment FUNCTION_RESULT : RESULT SEP? '(' SEP? IDENTIFIER SEP? ')';
fragment PROCEDURE_BIND : BIND SEP? '(' SEP? C SEP? (',' SEP? NAME SEP? '=' SEP? STRING SEP?)? ')';

fragment PROCEDURE_ARGS : '(' ((PROCEDURE_ARG (',' PROCEDURE_ARG)*) | SEP)? ')';
fragment PROCEDURE_ARG : SEP? IDENTIFIER SEP?;

fragment BASIC_TYPE_ID : INTEGER | REAL | DOUBLEPRECISION | COMPLEX | CHARACTER | LOGICAL;

fragment DOUBLE_PRECISION : DOUBLE SEP PRECISION;

fragment CHARACTER : C H A R A C T E R;
fragment COMPLEX : C O M P L E X;
fragment DOUBLEPRECISION : D O U B L E P R E C I S I O N;
fragment LOGICAL : L O G I C A L;
fragment INTEGER : I N T E G E R;
fragment REAL : R E A L;

fragment BIND : B I N D;
fragment DOUBLE : D O U B L E;
fragment PRECISION : P R E C I S I O N;
fragment ELEMENTAL: E L E M E N T A L;
fragment ENDFUNCTION : E N D F U N C T I O N;
fragment ENDPROGRAM : E N D P R O G R A M;
fragment ENDSUBROUTINE : E N D S U B R O U T I N E;
fragment END : E N D;
fragment FUNCTION : F U N C T I O N;
fragment NAME : N A M E;
fragment PURE : P U R E;
fragment RECURSIVE : R E C U R S I V E;
fragment RESULT : R E S U L T;
fragment SUBROUTINE : S U B R O U T I N E;
fragment TYPE : T Y P E;

fragment RENAME_LIST_STMT : RENAME_STMT (',' SEP? RENAME_STMT)*;
fragment RENAME_STMT : IDENTIFIER SEP? '=>' SEP? IDENTIFIER SEP?;

fragment ONLY_LIST_STMT : SEP? ONLY SEP? ':' SEP? ONLY_STMT (',' SEP? ONLY_STMT)*;
fragment ONLY_STMT : IDENTIFIER SEP? ('=>' SEP? IDENTIFIER SEP?)?;

fragment STRING : ((DQ (~'"' | QUOTED_DQ)* DQ) |
          (SQ (~'\'' | QUOTED_SQ)* SQ));
fragment QUOTED_DQ : DQ DQ;
fragment QUOTED_SQ : SQ SQ;

EOL : ('\r')? '\n' ->skip;
SEMICOLON : ';' ->skip;

fragment DQ : '"';
fragment SQ : '\'';

fragment DATA : D A T A;
fragment BLOCK : B L O C K;
fragment USE : U S E;
fragment MODULE : M O D U L E;
fragment ONLY : O N L Y;
fragment PROGRAM : P R O G R A M;

fragment IDENTIFIER : LETTER (LETTER | DIGIT | '_')*;

fragment LETTER : [a-zA-Z];
fragment DIGIT : [0-9];

fragment SEP : WS+;
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

