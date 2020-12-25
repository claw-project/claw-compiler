/**
 * ANTLR 4 Grammar file for parsing line with applied CLAW Ignore directive.
 *
 * @author Mikhail Zhigun
 * @copyright 2020, MeteoSwiss
 */
grammar CLAWIgnoreLineRecognizer;

root : contents EOF;

contents: CONTENTS;

CLAW_IGNORE_DIR_START: SEP? '!' SEP? '$' CLAW SEP IGNORE SEP ->skip;
CONTENTS : '|' NOT_EOL_CHR* '|';
CLAW_IGNORE_DIR_END : SEP? ('\r')? '\n' ->skip;

fragment NOT_EOL_CHR:  ~[\r\n];
fragment CLAW : C L A W;
fragment IGNORE : I G N O R E;
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

