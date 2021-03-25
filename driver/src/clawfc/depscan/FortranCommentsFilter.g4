/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 * @author Mikhail Zhigun
*/
/**
 * ANTLR 4 Grammar file for filtering out Fortran comments.
 */
grammar FortranCommentsFilter;

root : fortran_doc EOF;

fortran_doc : (string | comment | other)*;

string : STRING;
comment : COMMENT;
other : other_token | eol;
other_token : OTHER;
eol : EOL;

OTHER : (~[!"'\n])+;
COMMENT : EM (~'\n')*;
STRING : (DQ (~'"' | QUOTED_DQ)* DQ) |
                  (SQ (~'\'' | QUOTED_SQ)* SQ);
EOL : ('\r')? '\n';

fragment QUOTED_DQ : DQ DQ;
fragment QUOTED_SQ : SQ SQ;
fragment EM : '!';
fragment DQ : '"';
fragment SQ : '\'';
