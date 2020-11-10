/**
 * ANTLR 4 Grammar file for detecting CLAW directives.
 * dependencies scan and discards the rest.
 *
 * @author Mikhail Zhigun
 * @copyright 2020, MeteoSwiss
 */
grammar FortranCLAWScanner;

root : (claw_directive_line | claw_guard_line)* EOF;

claw_directive_line : CLAW_DIRECTIVE_LINE;
claw_guard_line : CLAW_GUARD_LINE;

CLAW_DIRECTIVE_LINE : SEP? '!' SEP? '$' CLAW (SEP (~'\n')*)?;
CLAW_GUARD_LINE :     SEP? '!' SEP? '$' (ACC | OMP) SEP CLAW SEP?;
OTHER_LINE : (~'\n')+ ->skip;

EOL : '\n' ->skip;

fragment ACC : A C C;
fragment CLAW : C L A W;
fragment OMP : O M P;
fragment SEP : WS+;
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

