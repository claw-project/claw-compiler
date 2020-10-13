/**
 * ANTLR 4 Grammar file for the Fortran dependencies parser. It only covers individual statements 
 * relevant to the extraction of dependencies information.
 *
 * @author Mikhail Zhigun
 * @copyright 2020, MeteoSwiss
 */
grammar FortranDepStatementsRecognizer;

module_open_stmt : MODULE module_open_name EOF;
module_open_name : identifier;

module_close_stmt : END MODULE module_close_name EOF;
module_close_name : identifier;

program_open_stmt : PROGRAM program_open_name EOF;
program_open_name : identifier;

program_close_stmt : END PROGRAM program_close_name EOF;
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
identifier : IDENTIFIER | ONLY | USE | MODULE | PROGRAM | END ;

USE : U S E;
MODULE : M O D U L E;
ONLY : O N L Y;
PROGRAM : P R O G R A M;
END : E N D;

IDENTIFIER : LETTER (LETTER | DIGIT | '_')*;

fragment LETTER : [a-zA-Z];
fragment DIGIT : [0-9];

SEP : WS+ -> skip;
fragment WS : [ \t\r];

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


