/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 * @author Mikhail Zhigun
*/
/**
 * ANTLR 4 Grammar file for removing CLAW Verbatim directives.
 */
grammar CLAWVerbatimScanner;

root : (claw_verbatim_line | other_line)* EOF;

claw_verbatim_line : CLAW_VERBATIM_LINE;
other_line : LINE;

CLAW_VERBATIM_LINE: SEP? '!' SEP? '$' CLAW SEP VERBATIM SEP LINE;
LINE : NOT_EOL_CHR* EOL;

fragment EOL : ('\r')? '\n';
fragment NOT_EOL_CHR:  ~[\r\n];

fragment CLAW : C L A W;
fragment VERBATIM : V E R B A T I M;
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

