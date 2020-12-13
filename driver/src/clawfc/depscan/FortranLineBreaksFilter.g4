/**
 * ANTLR 4 Grammar file for filtering out line breaks.
 *
 * @author Mikhail Zhigun
 * @copyright 2020, MeteoSwiss
 */
grammar FortranLineBreaksFilter;

root : fortran_text EOF;

fortran_text : (closed_line_break | unclosed_line_break | other | eol)*;
unclosed_line_break : UNCLOSED_LINE_BREAK;
closed_line_break : CLOSED_LINE_BREAK;
other : OTHER;
eol : EOL;

CLOSED_LINE_BREAK : UNCLOSED_LINE_BREAK '&';
//Unclosed line break cannot be skipped, because it acts like a separator in Fortran
UNCLOSED_LINE_BREAK : '&' (SEP? EOL)+ SEP?;
OTHER : (~[&\n])+ |
        '&' (~'\n')* ~[&\r\t\n] SEP? |
        '&';

EOL : ('\r')? '\n';

fragment SEP : WS+;
fragment WS : [ \t];