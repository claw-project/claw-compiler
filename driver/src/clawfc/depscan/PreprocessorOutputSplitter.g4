/**
 * ANTLR 4 Grammar file for the Preprocessed output splitter. Splits output text added by the preprocessor. Recognizes
 * line markers.
 *
 * @author Mikhail Zhigun
 * @copyright 2020, MeteoSwiss
 */
grammar PreprocessorOutputSplitter;

root: (non_preproc_line | preproc_line_marker_line | preproc_other_line)* EOF;

non_preproc_line : NON_PREPROC_LINE;

preproc_line_marker_line : PREPROC_LINE_MARKER_LINE;

preproc_other_line : PREPROC_OTHERLINE;

PREPROC_LINE_MARKER_LINE : '#' SEP NUMBER SEP '"' FILENAME_STRING '"' (SEP NUMBER)? SEP? EOL;
PREPROC_OTHERLINE : '#' (~'\n')* EOL;
NON_PREPROC_LINE : EOL | ~[#\n] (~'\n')* EOL;

fragment FILENAME_STRING : (~[\r\n"] | ESCAPED_DQ)+;
fragment ESCAPED_DQ : '\\"';

fragment NUMBER : DIGIT+;
fragment DIGIT : [0-9];

fragment SEP : WS+;
fragment WS : [ \t];
fragment EOL : '\r'? '\n';