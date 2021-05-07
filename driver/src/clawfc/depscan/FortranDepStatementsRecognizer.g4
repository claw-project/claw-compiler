/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 * @author Mikhail Zhigun
*/
/**
 * ANTLR 4 Grammar file for the Fortran dependencies parser. It only covers individual statements 
 * relevant to the extraction of dependencies information.
 */
grammar FortranDepStatementsRecognizer;

block_data_open_stmt : (BLOCKDATA | (BLOCK DATA)) block_data_open_name? EOF;
block_data_open_name : identifier;

block_data_close_stmt : ((END (((BLOCK DATA) | BLOCKDATA) block_data_close_name?)?) |
                        (ENDBLOCKDATA block_data_close_name?)) EOF;
block_data_close_name : identifier;

module_open_stmt : MODULE module_open_name EOF;
module_open_name : identifier;

module_close_stmt : ((END (MODULE module_close_name?)?) | (ENDMODULE module_close_name?)) EOF;
module_close_name : identifier;

program_open_stmt : PROGRAM program_open_name EOF;
program_open_name : identifier;

program_close_stmt : ((END (PROGRAM program_close_name?)?) | (ENDPROGRAM program_close_name?)) EOF;
program_close_name : identifier;

use_stmt : USE (',' module_nature)? '::'? use_module_name (',' (only ':')? rename_list_stmt?)? EOF;
use_module_name : identifier;

module_nature : MODULE_NATURE;
only : ONLY;

rename_list_stmt : rename_stmt (',' rename_stmt)*;
rename_stmt : use_symbol_name ('=>' use_symbol_name_from)?;

use_symbol_name : identifier | operator_name;
use_symbol_name_from : identifier | operator_name;

operator_name : OPERATOR_NAME;


// Lexer cannot differentiate between ambiguous keywords and identifiers
identifier : IDENTIFIER |
BLOCKDATA | BLOCK |
DATA |
ENDBLOCKDATA | ENDBLOCK |  ENDMODULE | ENDPROGRAM |
END |
FUNCTION  |
MODULE  |
ONLY  |
PROGRAM  |
USE |
MODULE_NATURE;

MODULE_NATURE : INTRINSIC | NON_INTRINSIC;

BLOCKDATA : B L O C K D A T A;
BLOCK : B L O C K;
DATA : D A T A;
ENDBLOCKDATA : E N D B L O C K D A T A;
ENDBLOCK : E N D B L O C K;
ENDMODULE : E N D M O D U L E;
ENDPROGRAM : E N D P R O G R A M;
END : E N D;
FUNCTION : F U N C T I O N;
MODULE : M O D U L E;
ONLY : O N L Y;
PROGRAM : P R O G R A M;
USE : U S E;

OPERATOR_NAME : OPERATOR '(' (~[)\r\n])+ ')' | ASSIGNMENT '(' '=' ')';

IDENTIFIER : LETTER (LETTER | DIGIT | '_')*;

fragment ASSIGNMENT : A S S I G N M E N T;
fragment INTRINSIC : I N T R I N S I C ;
fragment NON_INTRINSIC : N O N '_' I N T R I N S I C ;
fragment DIGIT : [0-9];
fragment LETTER : [a-zA-Z];
fragment OPERATOR : O P E R A T O R;

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
