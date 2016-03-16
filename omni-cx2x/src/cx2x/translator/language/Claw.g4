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
import cx2x.translator.common.Constant;
import cx2x.translator.pragma.*;
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


ids_list[List<String> ids]
  :
    i=IDENTIFIER { $ids.add($i.text); }
  | i=IDENTIFIER { $ids.add($i.text); } ',' ids_list[$ids]
;

directive[ClawLanguage language]:
    LFUSION { $language.setDirective(ClawDirective.LOOP_FUSION); } group_option[$language] EOF
  | LINTERCHANGE { $language.setDirective(ClawDirective.LOOP_INTERCHANGE); } indexes_option[$language] EOF
  | LEXTRACT range_option EOF
    {
      $language.setDirective(ClawDirective.LOOP_EXTRACT);
      $language.setRange($range_option.r);
    }
  | REMOVE { $language.setDirective(ClawDirective.REMOVE); } EOF
  | END REMOVE { $language.setDirective(ClawDirective.END_REMOVE); } EOF
;

group_option[ClawLanguage language]:
    GROUP '(' group_name=IDENTIFIER ')'
    { $language.setGroupOption($group_name.text); }
  | /* empty */
;

indexes_option[ClawLanguage language]
  @init{
    List<String> indexes = new ArrayList();
  }
  :
    '(' ids_list[indexes] ')' { $language.setIndexes(indexes); }
  | /* empty */
;

range_option returns [ClawRange r]
  @init{
    $r = new ClawRange();
  }
  :
    RANGE '(' induction=IDENTIFIER '=' lower=IDENTIFIER ',' upper=IDENTIFIER ')'
    {
      $r.setInductionVar($induction.text);
      $r.setLowerBound($lower.text);
      $r.setUpperBound($upper.text);
      $r.setStep(Constant.DEFAULT_STEP_VALUE);
    }
  | RANGE '(' induction=IDENTIFIER '=' lower=IDENTIFIER ',' upper=IDENTIFIER ',' step=IDENTIFIER ')'
    {
      $r.setInductionVar($induction.text);
      $r.setLowerBound($lower.text);
      $r.setUpperBound($upper.text);
      $r.setStep($step.text);
    }
;

mapping_var returns [ClawMappingVar mappingVar]:
    lhs=IDENTIFIER '/' rhs=IDENTIFIER
    {
      $mappingVar = new ClawMappingVar($lhs.text, $rhs.text);
    }
  | i=IDENTIFIER { $mappingVar = new ClawMappingVar($i.text, $i.text); }
;

mapping_var_list[List<ClawMappingVar> vars]:
     mv=mapping_var { $vars.add($mv.mappingVar); }
   | mv=mapping_var { $vars.add($mv.mappingVar); } ',' mapping_var_list[$vars]
;


/*
mapping_option returns [ClawMapping mapping]
  @init{
    $mapping = new ClawMapping();
  }
  :
    MAP '(' ids_list ':' ids_list ')'
;

map_list:
    mapping_option
  | mapping_option map_list
;
*/

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
IDENTIFIER      : [a-zA-Z_$0-9] [a-zA-Z_$0-9]* ;
NUMBER          : (DIGIT)+ ;
fragment DIGIT  : '0'..'9' ;


// Skip whitspaces
WHITESPACE   : ( '\t' | ' ' | '\r' | '\n'| '\u000C' )+ { skip(); };