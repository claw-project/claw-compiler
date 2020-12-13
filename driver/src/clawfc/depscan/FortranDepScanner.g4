/**
 * ANTLR 4 Grammar file for the Fortran dependencies scanner. Extracts statements relevant to the 
 * dependencies scan and discards the rest. 
 *
 * @author Mikhail Zhigun
 * @copyright 2020, MeteoSwiss
 */
grammar FortranDepScanner;

root : (module_block | program_block)* EOF;

module_block : module_open_stmt (use_stmt)* module_close_stmt;
program_block : program_open_stmt (use_stmt)* program_close_stmt;

module_open_stmt : MODULE_OPEN_STMT; 
module_close_stmt: MODULE_CLOSE_STMT;
program_open_stmt : PROGRAM_OPEN_STMT;
program_close_stmt : PROGRAM_CLOSE_STMT;
use_stmt : USE_STMT;

MODULE_OPEN_STMT : SEP? MODULE SEP IDENTIFIER SEP?;
MODULE_CLOSE_STMT : SEP? END SEP MODULE SEP IDENTIFIER SEP?;

PROGRAM_OPEN_STMT : SEP? PROGRAM SEP IDENTIFIER SEP?;
PROGRAM_CLOSE_STMT : SEP? END SEP PROGRAM SEP IDENTIFIER SEP?;

USE_STMT : SEP? USE SEP IDENTIFIER SEP? (',' SEP? (RENAME_LIST_STMT | ONLY_LIST_STMT))?;

fragment RENAME_LIST_STMT : RENAME_STMT (',' SEP? RENAME_STMT)*;
fragment RENAME_STMT : IDENTIFIER SEP? '=>' SEP? IDENTIFIER SEP?;

fragment ONLY_LIST_STMT : SEP? ONLY SEP? ':' SEP? ONLY_STMT (',' SEP? ONLY_STMT)*;
fragment ONLY_STMT : IDENTIFIER SEP? ('=>' SEP? IDENTIFIER SEP?)?;

STRING : ((DQ (~'"' | QUOTED_DQ)* DQ) |
          (SQ (~'\'' | QUOTED_SQ)* SQ)) ->skip;
fragment QUOTED_DQ : DQ DQ;
fragment QUOTED_SQ : SQ SQ;

OTHER : (~[\n;'"])+ ->skip;

EOL : ('\r')? '\n' ->skip;
SEMICOLON : ';' ->skip;

DQ : '"' ->skip;
SQ : '\'' ->skip;

fragment USE : U S E;
fragment MODULE : M O D U L E;
fragment ONLY : O N L Y;
fragment PROGRAM : P R O G R A M;
fragment END : E N D;

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

