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

use_stmt : USE use_module_name (',' (rename_list_stmt | only_list_stmt))? EOF;
use_module_name : identifier;

rename_list_stmt : rename_stmt (',' rename_stmt)*;
rename_stmt : use_symbol_name '=>' use_symbol_name_from;

use_symbol_name : identifier;
use_symbol_name_from : identifier;

only_list_stmt : ONLY  ':'  only_stmt (','  only_stmt)*;
only_stmt : use_only_symbol_name  ('=>'  use_only_symbol_name_from )?;

use_only_symbol_name : identifier;
use_only_symbol_name_from : identifier;

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
USE;

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
