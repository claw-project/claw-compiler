/**
 * ANTLR 4 Grammar file for filtering out trailing backslashes at the end of Fortran comment.
 *
 * @author Mikhail Zhigun
 * @copyright 2021, MeteoSwiss
 */
grammar CommentTrailingBackslashRecognizer;

root : other (other | bs)* (trailing_bs_seq | eol) EOF;

trailing_bs_seq : TRAILING_BS_SEQ;
other : OTHER;
bs : BS;
eol: EOL;

TRAILING_BS_SEQ : BS [\\ \t]* EOL;

OTHER : (~[\\\r\n])+;

BS : '\\';
EOL : ('\r')? '\n';
