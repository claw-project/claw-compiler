/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */


/**
 * ANTLR 4 Grammar file for the CLAW directive language.
 *
 * @author clementval
 */
grammar Claw;

@header
{
import java.util.List;
import java.util.ArrayList;
}

/*----------------------------------------------------------------------------
 * PARSER RULES
 *----------------------------------------------------------------------------*/


/*
 * Entry point for the analyzis of a CLAW directive.
 * Return a CLawLanguage object with all needed information.
 */
analyze returns [ClawLanguage language]
  @init{
    $language = new ClawLanguage();
  }
  :
  CLAW directive[$language]
;


index_list returns [List<String> indexes]
  @init{
    $indexes = new ArrayList();
  }
  :
    i=IDENTIFIER { $indexes.add($i.text); }
  | i=IDENTIFIER { $indexes.add($i.text); } COMMA index_list
;


directive[ClawLanguage language]:
    LFUSION { $language.setDirective(ClawDirective.LOOP_FUSION); } group_option
  | LINTERCHANGE { $language.setDirective(ClawDirective.LOOP_INTERCHANGE); } '(' index_list ')'
  | LEXTRACT { $language.setDirective(ClawDirective.LOOP_EXTRACT); } range_option
  | REMOVE { $language.setDirective(ClawDirective.REMOVE); }
  | END REMOVE { $language.setDirective(ClawDirective.END_REMOVE); }
;

group_option:
    GROUP '(' IDENTIFIER ')'
  | /* empty */
;

range_option:
    RANGE '(' IDENTIFIER '=' ')'
;



/*----------------------------------------------------------------------------
 * LEXER RULES
 *----------------------------------------------------------------------------*/

// Start point
CLAW         : 'claw';

// Directives
LFUSION      : 'loop-fusion';
LINTERCHANGE : 'loop-interchange';
LEXTRACT     : 'loop-extract';
REMOVE       : 'remove';
END          : 'end';

// Options
GROUP        : 'group';
RANGE        : 'range';
MAP          : 'map';

// Special elements
IDENTIFIER      : [a-zA-Z_$] [a-zA-Z_$0-9]* ;
COMMA           : ',' ;
COLON           : ':';
NUMBER          : (DIGIT)+ ;
fragment DIGIT  : '0'..'9' ;


// Skip whitspaces
WHITESPACE   : ( '\t' | ' ' | '\r' | '\n'| '\u000C' )+ { skip(); };