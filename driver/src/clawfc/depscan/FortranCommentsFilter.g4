/**
 * ANTLR 4 Grammar file for filtering out Fortran comments.
 *
 * @author Mikhail Zhigun
 * @copyright 2020, MeteoSwiss
 */
grammar FortranCommentsFilter;

root : fortran_doc EOF;

fortran_doc : (string | comment | other)*;

string : STRING;
comment : COMMENT;
other : OTHER;

OTHER : (~[!"'])+;
COMMENT : EM (~'\n')* EOL;
STRING : (DQ (~'"' | QUOTED_DQ)* DQ) |
                  (SQ (~'\'' | QUOTED_SQ)* SQ);

fragment QUOTED_DQ : DQ DQ;
fragment QUOTED_SQ : SQ SQ;
fragment EM : '!';
fragment DQ : '"';
fragment SQ : '\'';
fragment EOL : '\n';
