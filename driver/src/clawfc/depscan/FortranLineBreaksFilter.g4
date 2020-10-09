/**
 * ANTLR 4 Grammar file for filtering out line breaks.
 *
 * @author Mikhail Zhigun
 * @copyright 2020, MeteoSwiss
 */
grammar FortranLineBreaksFilter;

root : fortran_text EOF;

fortran_text : (unclosed_line_break | other)*;
unclosed_line_break : UNCLOSED_LINE_BREAK;
other : OTHER;

CLOSED_LINE_BREAK : UNCLOSED_LINE_BREAK '&' ->skip;
//Unclosed line break cannot be skipped, because it acts like a separator in Fortran
UNCLOSED_LINE_BREAK : '&' (SEP? EOL)+ SEP?;
OTHER : (~'&')+ |
        '&' (~'\n')* ~[&\r\t\n] SEP? EOL |
        '&';

fragment SEP : WS+;
fragment WS : [ \t\r];
fragment EOL : '\n';