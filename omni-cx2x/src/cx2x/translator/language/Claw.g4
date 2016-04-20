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
import cx2x.translator.misc.Utility;
}

/*----------------------------------------------------------------------------
 * PARSER RULES
 *----------------------------------------------------------------------------*/


/*
 * Entry point for the analyzis of a CLAW directive.
 * Return a CLawLanguage object with all needed information.
 */
analyze returns [ClawLanguage l]
  @init{
    $l = new ClawLanguage();
  }
  :
  CLAW directive[$l]
;


ids_list[List<String> ids]
  :
    i=IDENTIFIER { $ids.add($i.text); }
  | i=IDENTIFIER { $ids.add($i.text); } ',' ids_list[$ids]
;

directive[ClawLanguage l]
  @init{
    List<ClawMapping> m = new ArrayList<>();
    List<String> o = new ArrayList<>();
    List<Integer> i = new ArrayList<>();
  }
  :

  // loop-fusion directive
    LFUSION group_option[$l] collapse_optional[$l] EOF
    { $l.setDirective(ClawDirective.LOOP_FUSION); }

  // loop-interchange directive
  | LINTERCHANGE indexes_option[$l] parallel_optional[$l] acc_optional[$l] EOF
    { $l.setDirective(ClawDirective.LOOP_INTERCHANGE); }

  // loop-extract directive
  | LEXTRACT range_option mapping_option_list[m] fusion_optional[$l] parallel_optional[$l] acc_optional[$l] EOF
    {
      $l.setDirective(ClawDirective.LOOP_EXTRACT);
      $l.setRange($range_option.r);
      $l.setMappings(m);
    }

  // remove directive
  | REMOVE EOF
    { $l.setDirective(ClawDirective.REMOVE); }
  | END REMOVE EOF
    {
      $l.setDirective(ClawDirective.REMOVE);
      $l.setEndPragma();
    }

  // Kcache directive
  | KCACHE data_optional[$l] offset_list_optional[i] private_optional[$l] EOF
    {
      $l.setDirective(ClawDirective.KCACHE);
      $l.setOffsets(i);
    }
  | KCACHE offset_list_optional[i] INIT private_optional[$l] EOF
    {
      $l.setDirective(ClawDirective.KCACHE);
      $l.setOffsets(i);
      $l.setInitClause();
    }

  // Array notation transformation directive
  | ARRAY_TRANS induction_optional[$l] fusion_optional[$l] parallel_optional[$l] acc_optional[$l] EOF
    {  $l.setDirective(ClawDirective.ARRAY_TRANSFORM); }
  | END ARRAY_TRANS
    {
      $l.setDirective(ClawDirective.ARRAY_TRANSFORM);
      $l.setEndPragma();
    }

  // loop-hoist directive
  | LHOIST '(' ids_list[o] ')' reshape_optional[$l] interchange_optional[$l] EOF
    {
      $l.setHoistInductionVars(o);
      $l.setDirective(ClawDirective.LOOP_HOIST);
    }
  | END LHOIST EOF
    {
      $l.setDirective(ClawDirective.LOOP_HOIST);
      $l.setEndPragma();
    }

  | ARRAY_TO_CALL array_name=IDENTIFIER '=' fct_name=IDENTIFIER '(' identifiers_list[o] ')'
    {
      $l.setDirective(ClawDirective.ARRAY_TO_CALL);
      $l.setFctParams(o);
      $l.setFctName($fct_name.text);
      $l.setArrayName($array_name.text);
    }
;

group_option[ClawLanguage l]:
    GROUP '(' group_name=IDENTIFIER ')'
    { $l.setGroupClause($group_name.text); }
  | /* empty */
;

collapse_optional[ClawLanguage l]:
    COLLAPSE '(' n=NUMBER ')'
    { $l.setCollapseClause($n.text); }
  | /* empty */
;

fusion_optional[ClawLanguage l]:
    FUSION group_option[$l] { $l.setFusionClause(); }
  | /* empty */
;

parallel_optional[ClawLanguage l]:
    PARALLEL { $l.setParallelClause(); }
  | /* empty */
;

acc_optional[ClawLanguage l]
  @init{
    List<String> tempAcc = new ArrayList<>();
  }
  :
    ACC '(' identifiers[tempAcc] ')' { $l.setAcceleratorClauses(Utility.join(" ", tempAcc)); }
  | /* empty */
;

interchange_optional[ClawLanguage l]:
    INTERCHANGE indexes_option[$l]
    {
      $l.setInterchangeClause();
    }
  | /* empty */
;

induction_optional[ClawLanguage l]
  @init{
    List<String> temp = new ArrayList<>();
  }
  :
    INDUCTION '(' ids_list[temp] ')' { $l.setInductionClause(temp); }
  | /* empty */
;

data_optional[ClawLanguage l]
  @init {
    List<String> temp = new ArrayList<>();
  }
  :
    DATA '(' ids_list[temp] ')' { $l.setDataClause(temp); }
  | /* empty */
;

private_optional[ClawLanguage l]:
    PRIVATE { $l.setPrivateClause(); }
  | /* empty */
;

reshape_optional[ClawLanguage l]
  @init{
    List<ClawReshapeInfo> r = new ArrayList();
  }
  :
    RESHAPE '(' reshape_list[r] ')'
    { $l.setReshapeClauseValues(r); }
  | /* empty */
;

reshape_element returns [ClawReshapeInfo i]
  @init{
    List<String> temp = new ArrayList();
  }
:
    array_name=IDENTIFIER '(' target_dim=NUMBER ')'
    { $i = new ClawReshapeInfo($array_name.text, Integer.parseInt($target_dim.text), temp); }
  | array_name=IDENTIFIER '(' target_dim=NUMBER ',' identifiers_list[temp] ')'
    { $i = new ClawReshapeInfo($array_name.text, Integer.parseInt($target_dim.text), temp); }
;

reshape_list[List<ClawReshapeInfo> r]:
    info=reshape_element { $r.add($info.i); } ',' reshape_list[$r]
  | info=reshape_element { $r.add($info.i); }
;

identifiers[List<String> ids]:
    i=IDENTIFIER { $ids.add($i.text); }
  | i=IDENTIFIER { $ids.add($i.text); } identifiers[$ids]
;

identifiers_list[List<String> ids]:
    i=IDENTIFIER { $ids.add($i.text); }
  | i=IDENTIFIER { $ids.add($i.text); } ',' identifiers_list[$ids]
;


indexes_option[ClawLanguage l]
  @init{
    List<String> indexes = new ArrayList();
  }
  :
    '(' ids_list[indexes] ')' { $l.setIndexes(indexes); }
  | /* empty */
;

offset_list_optional[List<Integer> offsets]:
    offset_list[$offsets]
  | /* empty */
;

offset_list[List<Integer> offsets]:
    offset[$offsets]
  | offset[$offsets] offset_list[$offsets]
;

offset[List<Integer> offsets]:
    n=NUMBER { $offsets.add(Integer.parseInt($n.text)); }
  | '-' n=NUMBER { $offsets.add(-Integer.parseInt($n.text)); }
  | '+' n=NUMBER { $offsets.add(Integer.parseInt($n.text)); }
;


range_option returns [ClawRange r]
  @init{
    $r = new ClawRange();
  }
  :
    RANGE '(' induction=IDENTIFIER '=' lower=range_id ',' upper=range_id ')'
    {
      $r.setInductionVar($induction.text);
      $r.setLowerBound($lower.text);
      $r.setUpperBound($upper.text);
      $r.setStep(Constant.DEFAULT_STEP_VALUE);
    }
  | RANGE '(' induction=IDENTIFIER '=' lower=range_id ',' upper=range_id ',' step=range_id ')'
    {
      $r.setInductionVar($induction.text);
      $r.setLowerBound($lower.text);
      $r.setUpperBound($upper.text);
      $r.setStep($step.text);
    }
;

range_id returns [String text]:
    n=NUMBER { $text = $n.text; }
  | i=IDENTIFIER { $text = $i.text; }
;


mapping_var returns [ClawMappingVar mappingVar]:
    lhs=IDENTIFIER '/' rhs=IDENTIFIER
    {
      $mappingVar = new ClawMappingVar($lhs.text, $rhs.text);
    }
  | i=IDENTIFIER
    {
      $mappingVar = new ClawMappingVar($i.text, $i.text);
    }
;


mapping_var_list[List<ClawMappingVar> vars]:
     mv=mapping_var { $vars.add($mv.mappingVar); }
   | mv=mapping_var { $vars.add($mv.mappingVar); } ',' mapping_var_list[$vars]
;


mapping_option returns [ClawMapping mapping]
  @init{
    $mapping = new ClawMapping();
    List<ClawMappingVar> listMapped = new ArrayList<ClawMappingVar>();
    List<ClawMappingVar> listMapping = new ArrayList<ClawMappingVar>();
    $mapping.setMappedVariables(listMapped);
    $mapping.setMappingVariables(listMapping);
  }
  :
    MAP '(' mapping_var_list[listMapped] ':' mapping_var_list[listMapping] ')'
;

mapping_option_list[List<ClawMapping> mappings]:
    m=mapping_option { $mappings.add($m.mapping); }
  | m=mapping_option { $mappings.add($m.mapping); } mapping_option_list[$mappings]
;

/*----------------------------------------------------------------------------
 * LEXER RULES
 *----------------------------------------------------------------------------*/

// Start point
CLAW         : 'claw';

// Directives
ARRAY_TRANS  : 'array-transform';
ARRAY_TO_CALL: 'call';
END          : 'end';
KCACHE       : 'kcache';
LEXTRACT     : 'loop-extract';
LFUSION      : 'loop-fusion';
LHOIST       : 'loop-hoist';
LINTERCHANGE : 'loop-interchange';
REMOVE       : 'remove';

// Clauses
ACC          : 'acc';
COLLAPSE     : 'collapse';
DATA         : 'data';
FUSION       : 'fusion';
GROUP        : 'group';
INDUCTION    : 'induction';
INIT         : 'init';
INTERCHANGE  : 'interchange';
MAP          : 'map';
PARALLEL     : 'parallel';
PRIVATE      : 'private';
RANGE        : 'range';
RESHAPE      : 'reshape';

// Special elements
IDENTIFIER      : [a-zA-Z_$] [a-zA-Z_$0-9-]* ;
NUMBER          : (DIGIT)+ ;
fragment DIGIT  : [0-9] ;

// Skip whitspaces
WHITESPACE   : ( '\t' | ' ' | '\r' | '\n'| '\u000C' )+ { skip(); };