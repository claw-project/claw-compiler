/**
 * ANTLR 4 Grammar file for the Fortran dependencies scanner.
 *
 * @author Mikhail Zhigun
 * @copyright 2020, MeteoSwiss
 */
grammar FortranDepScanner;

root : PROC_STMT EOF;

PROC_STMT : SEP? (MODULE_OPEN_STMT | MODULE_CLOSE_STMT | PROGRAM_OPEN_STMT | PROGRAM_CLOSE_STMT | USE_STMT);

MODULE_OPEN_STMT : MODULE SEP IDENTIFIER SEP?;
MODULE_CLOSE_STMT : END SEP MODULE SEP IDENTIFIER SEP?;

PROGRAM_OPEN_STMT : PROGRAM SEP IDENTIFIER SEP?;
PROGRAM_CLOSE_STMT : END SEP PROGRAM SEP IDENTIFIER SEP?;

USE_STMT : USE SEP IDENTIFIER SEP? (',' SEP? (RENAME_LIST_STMT | ONLY_LIST_STMT))?;

RENAME_LIST_STMT : RENAME_STMT (',' SEP? RENAME_STMT)*;
RENAME_STMT : IDENTIFIER SEP? '=>' SEP? IDENTIFIER SEP?;

ONLY_LIST_STMT : ONLY SEP? ':' SEP? ONLY_STMT (',' SEP? ONLY_STMT)*;
ONLY_STMT : IDENTIFIER SEP? ('=>' SEP? IDENTIFIER SEP?)?;

fragment USE : U S E;
fragment MODULE : M O D U L E;
fragment ONLY : O N L Y;
fragment PROGRAM : P R O G R A M;
fragment END : E N D;

IDENTIFIER : LETTER (LETTER | DIGIT | '_')*;

fragment LETTER : [a-zA-Z];
fragment DIGIT : [0-9];

fragment SEP : WS+;
fragment WS : [ \t\r];
fragment EOL : '\n';

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

ERROR_CHAR : .;

