/**
 * ANTLR 4 Grammar file for parsing Fortran include statement.
 *
 * @author Mikhail Zhigun
 * @copyright 2020, MeteoSwiss
 */
grammar FortranIncludeStatementRecognizer;

root : include_statement_line EOF;

include_statement_line : SEP? INCLUDE SEP include_string SEP? EOL;
include_string : INCLUDE_STRING;

INCLUDE_STRING : (DQ (~'"' | QUOTED_DQ)* DQ)
               | (SQ (~'\'' | QUOTED_SQ)* SQ);

INCLUDE : I N C L U D E;
SEP : WS+;
EOL : ('\r')? '\n';

fragment WS : [ \t];
fragment QUOTED_DQ : DQ DQ;
fragment QUOTED_SQ : SQ SQ;
fragment DQ : '"';
fragment SQ : '\'';

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
