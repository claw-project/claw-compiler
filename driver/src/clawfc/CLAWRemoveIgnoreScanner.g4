/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 * @author Mikhail Zhigun
*/
/**
 * ANTLR 4 Grammar file for recognizing applied CLAW Ignore directives.
 */
grammar CLAWRemoveIgnoreScanner;

root : (claw_ignore_escaped_line | other_line)* EOF;

claw_ignore_escaped_line : CLAW_IGNORE_ESCAPED_LINE;
other_line : OTHER_LINE;

CLAW_IGNORE_START_LINE : CLAW_IGNORE_DIR_START IGNORE SEP? EOL ->skip;
CLAW_IGNORE_ESCAPED_LINE : CLAW_IGNORE_DIR_START IGNORE SEP '|' NOT_EOL_CHR* '|' SEP? EOL;
CLAW_IGNORE_END_LINE : CLAW_IGNORE_DIR_START END SEP IGNORE SEP? EOL ->skip;
OTHER_LINE : NOT_EOL_CHR* EOL;

fragment CONTENTS : '|' NOT_EOL_CHR* '|';
fragment EOL : ('\r')? '\n';
fragment NOT_EOL_CHR:  ~[\r\n];

fragment CLAW_IGNORE_DIR_START: SEP? '!' SEP? '$' CLAW SEP;
fragment CLAW : C L A W;
fragment IGNORE : I G N O R E;
fragment END : E N D;
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

