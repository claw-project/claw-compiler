/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 * @author Mikhail Zhigun
*/
/**
 * ANTLR 4 Grammar file for parsign file paths out of preprocessor line markers.
 */
grammar PreprocessorLineMarkerRecognizer;

root: preproc_line_marker_line EOF;

preproc_line_marker_line : filename_string;
filename_string : FILENAME_STRING;

LINE_START : '#' SEP NUMBER SEP '"' ->skip;
FILENAME_STRING : (~[\r\n"] | ESCAPED_DQ)+;
LINE_END : '"' (SEP NUMBER)? SEP? EOL ->skip;

fragment NUMBER : DIGIT+;
fragment ESCAPED_DQ : '\\"';

fragment DIGIT : [0-9];

fragment SEP : WS+;
fragment WS : [ \t];
fragment EOL : '\r'? '\n';