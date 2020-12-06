/**
 * ANTLR 4 Grammar file for detecting Fortran include statements.
 *
 * @author Mikhail Zhigun
 * @copyright 2020, MeteoSwiss
 */
grammar FortranIncludesResolver;

root : (include_line)* EOF;

include_line : INCLUDE_STATEMENT_LINE;

INCLUDE_STATEMENT_LINE : SEP? INCLUDE SEP INCLUDE_STRING SEP? EOL;
OTHER_LINE : (~'\n')* EOL ->skip;

fragment INCLUDE_STRING : (DQ (~'"' | QUOTED_DQ)* DQ)
                | (SQ (~'\'' | QUOTED_SQ)* SQ);

fragment SEP : WS+;
fragment WS : [ \t\r];
fragment EOL : '\n';
fragment QUOTED_DQ : DQ DQ;
fragment QUOTED_SQ : SQ SQ;
fragment DQ : '"';
fragment SQ : '\'';
fragment INCLUDE : I N C L U D E;

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

