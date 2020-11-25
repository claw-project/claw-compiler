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
other : OTHER | EOL;

OTHER : (~[!"'\n])+;
COMMENT : EM (~'\n')*;
STRING : (DQ (~'"' | QUOTED_DQ)* DQ) |
                  (SQ (~'\'' | QUOTED_SQ)* SQ);
EOL : '\n';

fragment QUOTED_DQ : DQ DQ;
fragment QUOTED_SQ : SQ SQ;
fragment EM : '!';
fragment DQ : '"';
fragment SQ : '\'';
