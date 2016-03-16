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


ids_list returns [List<String> ids]
  @init{
    $ids = new ArrayList();
  }
  :
    i=IDENTIFIER { $ids.add($i.text); }
  | i=IDENTIFIER { $ids.add($i.text); } ',' ids_list
;


directive[ClawLanguage language]:
    LFUSION { $language.setDirective(ClawDirective.LOOP_FUSION); } group_option[$language]
  | LINTERCHANGE { $language.setDirective(ClawDirective.LOOP_INTERCHANGE); } '(' ids_list ')'
  | LEXTRACT { $language.setDirective(ClawDirective.LOOP_EXTRACT); } range_option
  | REMOVE { $language.setDirective(ClawDirective.REMOVE); }
  | END REMOVE { $language.setDirective(ClawDirective.END_REMOVE); }
;

group_option[ClawLanguage language]:
    GROUP '(' group_name=IDENTIFIER ')'
    { $language.setGroupOption($group_name.text); }
  | /* empty */
;

range_option:
    RANGE '(' induction=IDENTIFIER '=' ',' lower=IDENTIFIER ',' upper=IDENTIFIER ')'
  | RANGE '(' induction=IDENTIFIER '=' ',' lower=IDENTIFIER ',' upper=IDENTIFIER ',' step=IDENTIFIER ')'
;

mapping_option:
    MAP '(' ids_list ':' ids_list ')'
;

map_list:
    mapping_option
  | mapping_option map_list
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
NUMBER          : (DIGIT)+ ;
fragment DIGIT  : '0'..'9' ;


// Skip whitspaces
WHITESPACE   : ( '\t' | ' ' | '\r' | '\n'| '\u000C' )+ { skip(); };