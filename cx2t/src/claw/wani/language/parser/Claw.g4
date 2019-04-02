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
/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */

import claw.wani.ClawConstant;
import claw.wani.language.*;

import claw.tatsu.common.*;
import claw.tatsu.directive.common.DataMovement;
import claw.tatsu.xcodeml.abstraction.*;
}

/*----------------------------------------------------------------------------
 * PARSER RULES
 *----------------------------------------------------------------------------*/


/*
 * Entry point for the analyzis of a CLAW directive.
 * Return a ClawPragma object with all needed information.
 */
analyze returns [ClawPragma l]
  @init{
    $l = new ClawPragma();
  }
  :
    CLAW directive[$l]
;

directive[ClawPragma l]
  @init{
    List<ClawMapping> m = new ArrayList<>();
    List<String> o = new ArrayList<>();
    List<String> s = new ArrayList<>();
  }
  :

  // loop-fusion directive
    LOOP_FUSION loop_fusion_clauses[$l] EOF
    { $l.setDirective(ClawDirective.LOOP_FUSION); }

  // loop-interchange directive
  | LOOP_INTERCHANGE loop_interchange_clauses[$l] EOF
    { $l.setDirective(ClawDirective.LOOP_INTERCHANGE); }

  // loop-extract directive
  | LOOP_EXTRACT range_option mapping_option_list[m]
    loop_extract_clauses[$l] EOF
    {
      $l.setDirective(ClawDirective.LOOP_EXTRACT);
      $l.setRange($range_option.r);
      $l.setMappings(m);
    }

  // remove directive
  | REMOVE (target_clause[$l])? EOF
    { $l.setDirective(ClawDirective.REMOVE); }
  | END REMOVE EOF
    {
      $l.setDirective(ClawDirective.REMOVE);
      $l.setEndPragma();
    }

  // Kcache directive
  | KCACHE data_clause[$l] kcache_clauses[$l] EOF
    {
      $l.setDirective(ClawDirective.KCACHE);
    }

  // Array notation transformation directive
  | EXPAND expand_clauses[$l] EOF
    {  $l.setDirective(ClawDirective.EXPAND); }
  | END EXPAND
    {
      $l.setDirective(ClawDirective.EXPAND);
      $l.setEndPragma();
    }

  // loop-hoist directive
  | LOOP_HOIST '(' ids_list[o] ')' loop_hoist_clauses[$l] EOF
    {
      $l.setValues(ClawClause.HOIST_INDUCTIONS, o);
      $l.setDirective(ClawDirective.LOOP_HOIST);
    }
  | END LOOP_HOIST EOF
    {
      $l.setDirective(ClawDirective.LOOP_HOIST);
      $l.setEndPragma();
    }

  // on the fly directive
  | ARRAY_TO_CALL array_name=IDENTIFIER '=' fct_name=IDENTIFIER
    '(' identifiers_list[o] ')' (target_clause[$l])? EOF
    {
      $l.setDirective(ClawDirective.ARRAY_TO_CALL);
      $l.setValues(ClawClause.FCT_PARAMETERS, o);
      $l.setValue(ClawClause.FCT_NAME, $fct_name.text);
      $l.setValue(ClawClause.ARRAY_NAME, $array_name.text);
    }

   // SCA (parallelize deprecated) directive
   | define_option[$l]+ PARALLELIZE data_over_clause[$l]*
     sca_clauses[$l] EOF
     {
       // TODO to be removed
       System.err.
       println("\"parallelize\" directive is deprecated. Use \"sca\" instead");
       $l.setDirective(ClawDirective.SCA);
       $l.getLocalModelConfig().generateDefaultLayout();
     }
   | PARALLELIZE FORWARD foward_clauses[$l] EOF
     {
       // TODO to be removed
       System.err.
       println("\"parallelize\" directive is deprecated. Use \"sca\" instead");
       $l.setDirective(ClawDirective.SCA);
       $l.setClause(ClawClause.FORWARD);
     }
   | END PARALLELIZE EOF
     {
       // TODO to be removed
       System.err.
       println("\"parallelize\" directive is deprecated. Use \"sca\" instead");
       $l.setDirective(ClawDirective.SCA);
       $l.setEndPragma();
     }

   // SCA directive using model configuration file.
   | SCA EOF
     {
       $l.setDirective(ClawDirective.SCA);
       $l.setScaModelConfig();
     }
   | SCA ROUTINE EOF
     {
       $l.setDirective(ClawDirective.SCA);
       $l.setClause(ClawClause.ROUTINE);
     }
   // SCA directive with define dimension
   | define_option[$l]+ SCA data_over_clause[$l]* sca_clauses[$l] EOF
     {
       $l.setDirective(ClawDirective.SCA);
       $l.getLocalModelConfig().generateDefaultLayout();
     }
   | SCA FORWARD foward_clauses[$l] EOF
     {
       $l.setDirective(ClawDirective.SCA);
       $l.setClause(ClawClause.FORWARD);
     }
   | END SCA EOF
     {
       $l.setDirective(ClawDirective.SCA);
       $l.setEndPragma();
     }

   // SCA model-data directive
   | MODEL_DATA EOF
     {
       $l.setDirective(ClawDirective.MODEL_DATA);
     }
   | MODEL_DATA LAYOUT '(' layout_id=IDENTIFIER ')' EOF
     {
       $l.setDirective(ClawDirective.MODEL_DATA);
       $l.setValue(ClawClause.LAYOUT, $layout_id.text);
     }
   | END MODEL_DATA EOF
     {
       $l.setDirective(ClawDirective.MODEL_DATA);
       $l.setEndPragma();
     }

   // ignore directive
   | IGNORE EOF
     {
       $l.setDirective(ClawDirective.IGNORE);
     }
   | END IGNORE EOF
     {
       $l.setDirective(ClawDirective.IGNORE);
       $l.setEndPragma();
     }

   |
     IF_EXTRACT EOF
     { $l.setDirective(ClawDirective.IF_EXTRACT); }

   | NODEP
     { $l.setDirective(ClawDirective.NO_DEP); }

   // autoport directive
   | AUTOPORT EOF
     { $l.setDirective(ClawDirective.AUTOPORT); }
   | END AUTOPORT EOF
     {
       $l.setDirective(ClawDirective.AUTOPORT);
       $l.setEndPragma();
     }

   // Special directives

   | VERBATIM // this directive accept anything after the verbatim
     { $l.setDirective(ClawDirective.VERBATIM); }
     REMAINING*
   | ACC  // this directive accept anything after the acc
     { $l.setDirective(ClawDirective.PRIMITIVE);  }
     REMAINING*
   | OMP // this directive accept anything after the omp
     { $l.setDirective(ClawDirective.PRIMITIVE); }
     REMAINING*
;

// Comma-separated identifiers list
ids_list[List<String> ids]
  :
    i=IDENTIFIER { $ids.add($i.text); }
  | i=IDENTIFIER { $ids.add($i.text); } ',' ids_list[$ids]
;

// Comma-separated identifiers or colon symbol list
ids_or_colon_list[List<String> ids]
  :
    i=IDENTIFIER { $ids.add($i.text); }
  | ':' { $ids.add(":"); }
  | i=IDENTIFIER { $ids.add($i.text); } ',' ids_or_colon_list[$ids]
  | ':' { $ids.add(":"); } ',' ids_or_colon_list[$ids]
;

// data over clause used in one_column directive
data_over_clause[ClawPragma l]
  @init{
    List<String> overLst = new ArrayList<>();
    List<String> dataLst = new ArrayList<>();
  }
:
    SCALAR '(' ids_list[dataLst] ')'
    {
      $l.setValues(ClawClause.SCALAR, dataLst);
    }
  | DATA '(' ids_list[dataLst] ')' OVER '(' ids_or_colon_list[overLst] ')'
    {
      $l.processDataOverClauses(dataLst, overLst);
    }
  | NOPROMOTE '(' ids_list[dataLst] ')'
    {
      $l.setValues(ClawClause.NO_PROMOTE, dataLst);
    }
;

// group clause
group_clause[ClawPragma l]:
    GROUP '(' group_name=IDENTIFIER ')'
    { $l.setValue(ClawClause.GROUP, $group_name.text); }
;

// collapse clause
collapse_clause[ClawPragma l]:
    COLLAPSE '(' n=NUMBER ')'
    { $l.setCollapseClause($n.text); }
;

// fusion clause
fusion_clause[ClawPragma l]:
    FUSION { $l.setClause(ClawClause.FUSION); }
    fusion_options[$l]
;

fusion_options[ClawPragma l]:
  (
      { !$l.hasClause(ClawClause.GROUP) }?      group_clause[$l]
    | { !$l.hasClause(ClawClause.COLLAPSE) }?   collapse_clause[$l]
    | { !$l.hasClause(ClawClause.CONSTRAINT) }? constraint_clause[$l]
  )*
;

// parallel clause
parallel_clause[ClawPragma l]:
    PARALLEL { $l.setClause(ClawClause.PARALLEL); }
;

// acc clause
acc_clause[ClawPragma l]
  @init{
    List<String> tempAcc = new ArrayList<>();
  }
  :
    ACC '(' identifiers[tempAcc] ')'
    { $l.setValue(ClawClause.ACC, String.join(" ", tempAcc)); }
;

// interchange clause
interchange_clause[ClawPragma l]:
    INTERCHANGE indexes_option[$l] { $l.setClause(ClawClause.INTERCHANGE); }
;

// induction clause
induction_clause[ClawPragma l]
  @init{
    List<String> temp = new ArrayList<>();
  }
  :
    INDUCTION '(' ids_list[temp] ')' {
        $l.setValues(ClawClause.INDUCTION, temp);
    }
;

// data clause
data_clause[ClawPragma l]
  @init {
    List<String> temp = new ArrayList<>();
  }
  :
    DATA '(' ids_list[temp] ')' { $l.setValues(ClawClause.DATA, temp); }
;

// private clause
private_clause[ClawPragma l]:
    PRIVATE { $l.setClause(ClawClause.PRIVATE); }
;

// reshape clause
reshape_clause[ClawPragma l]
  @init{
    List<ReshapeInfo> r = new ArrayList();
  }
  :
    RESHAPE '(' reshape_list[r] ')'
    { $l.setReshapeClauseValues(r); }
;

// cleanup clause
cleanup_clause[ClawPragma l]:
    CLEANUP { $l.setCleanupClauseValue(CompilerDirective.NONE); }
  | CLEANUP '(' OMP ')' { $l.setCleanupClauseValue(CompilerDirective.OPENMP); }
  | CLEANUP '(' ACC ')' { $l.setCleanupClauseValue(CompilerDirective.OPENACC); }
;

// reshape clause
reshape_element returns [ReshapeInfo i]
  @init{
    List<Integer> temp = new ArrayList();
  }
:
    array_name=IDENTIFIER '(' target_dim=NUMBER ')'
    {
      $i = new ReshapeInfo($array_name.text,
      Integer.parseInt($target_dim.text), temp);
    }
  | array_name=IDENTIFIER '(' target_dim=NUMBER ',' integers_list[temp] ')'
    {
      $i = new ReshapeInfo($array_name.text,
      Integer.parseInt($target_dim.text), temp);
    }
;

reshape_list[List<ReshapeInfo> r]:
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

integers[List<Integer> ints]:

;

integers_list[List<Integer> ints]:
    i=NUMBER { $ints.add(Integer.parseInt($i.text)); }
  | i=NUMBER { $ints.add(Integer.parseInt($i.text)); } ',' integers[$ints]
;

indexes_option[ClawPragma l]
  @init{
    List<String> indexes = new ArrayList();
  }
  :
    '(' ids_list[indexes] ')' {
        $l.setValues(ClawClause.INTERCHANGE_INDEXES, indexes);
     }
  | /* empty */
;

offset_clause[List<Integer> offsets]:
    OFFSET '(' offset_list[$offsets] ')'
;

offset_list[List<Integer> offsets]:
    offset[$offsets]
  | offset[$offsets] ',' offset_list[$offsets]
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
    RANGE '(' induction_var=IDENTIFIER '=' lower=range_id ',' upper=range_id ')'
    {
      $r.setInductionVar($induction_var.text);
      $r.setLowerBound($lower.text);
      $r.setUpperBound($upper.text);
      $r.setStep(ClawConstant.DEFAULT_STEP_VALUE);
    }
  | RANGE '(' induction_var=IDENTIFIER '=' lower=range_id ',' upper=range_id ','
    step=range_id ')'
    {
      $r.setInductionVar($induction_var.text);
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
  | m=mapping_option { $mappings.add($m.mapping); }
    mapping_option_list[$mappings]
;


define_option[ClawPragma l]:
    DEFINE DIMENSION id=IDENTIFIER '(' lower=range_id ':' upper=range_id ')'
    {
      DimensionDefinition cd =
        new DimensionDefinition($id.text, $lower.text, $upper.text);
      $l.addDimension(cd);
    }
;

// Allow to switch order
sca_clauses[ClawPragma l]:
  (
    { !$l.hasClause(ClawClause.COPY) }?   copy_clause[$l]
  | { !$l.hasClause(ClawClause.UPDATE) }? update_clause[$l]
  | { !$l.hasClause(ClawClause.CREATE) }? create_clause[$l]
  )*
;

// Allow to switch order
foward_clauses[ClawPragma l]:
  (
    { !$l.hasClause(ClawClause.COPY) }?   copy_clause[$l]
  | { !$l.hasClause(ClawClause.UPDATE) }? update_clause[$l]
  | { !$l.hasClause(ClawClause.CREATE) }? create_clause[$l]
  | { !$l.hasClause(ClawClause.PARALLEL) }? parallel_clause[$l]
  )*
;

copy_clause[ClawPragma l]:
    COPY
    { $l.setCopyClauseValue(DataMovement.TWO_WAY); }
  | COPY '(' IN ')'
    { $l.setCopyClauseValue(DataMovement.HOST_TO_DEVICE); }
  | COPY '(' OUT ')'
    { $l.setCopyClauseValue(DataMovement.DEVICE_TO_HOST); }
;

update_clause[ClawPragma l]:
    UPDATE
    { $l.setUpdateClauseValue(DataMovement.TWO_WAY); }
  | UPDATE '(' IN ')'
    { $l.setUpdateClauseValue(DataMovement.HOST_TO_DEVICE); }
  | UPDATE '(' OUT ')'
    { $l.setUpdateClauseValue(DataMovement.DEVICE_TO_HOST); }
;

create_clause[ClawPragma l]:
    CREATE
    { $l.setClause(ClawClause.CREATE); }
;

target_clause[ClawPragma l]
  @init{
    List<Target> targets = new ArrayList<>();
  }
  :
    TARGET '(' target_list[targets] ')'
    { $l.setTargetClauseValue(targets); }
;

constraint_clause[ClawPragma l]:
    CONSTRAINT '(' NONE ')'
    { $l.setConstraintClauseValue(ClawConstraint.NONE); }
  | CONSTRAINT '(' DIRECT ')'
    { $l.setConstraintClauseValue(ClawConstraint.DIRECT); }
;

target_list[List<Target> targets]:
    target { if(!$targets.contains($target.t)) { $targets.add($target.t); } }
  | target { if(!$targets.contains($target.t)) { $targets.add($target.t); } }
    ',' target_list[$targets]
;


target returns [Target t]:
    CPU { $t = Target.CPU; }
  | GPU { $t = Target.GPU; }
  | MIC { $t = Target.MIC; }
;

// Possible permutation of clauses for the loop-fusion directive
loop_fusion_clauses[ClawPragma l]:
  (
    { !$l.hasClause(ClawClause.GROUP) }?      group_clause[$l]
  | { !$l.hasClause(ClawClause.COLLAPSE) }?   collapse_clause[$l]
  | { !$l.hasClause(ClawClause.TARGET) }?     target_clause[$l]
  | { !$l.hasClause(ClawClause.CONSTRAINT) }? constraint_clause[$l]
  )*
;

// Possible permutation of clauses for the loop-interchange directive
loop_interchange_clauses[ClawPragma l]:
  indexes_option[$l]
  (
    { !$l.hasClause(ClawClause.PARALLEL) }?    parallel_clause[$l]
  | { !$l.hasClause(ClawClause.ACC) }? acc_clause[$l]
  | { !$l.hasClause(ClawClause.TARGET) }?      target_clause[$l]
  )*
;

// Possible permutation of clauses for the loop-extract directive
loop_extract_clauses[ClawPragma l]:
  (
    { !$l.hasClause(ClawClause.FUSION) }?      fusion_clause[$l]
  | { !$l.hasClause(ClawClause.PARALLEL) }?    parallel_clause[$l]
  | { !$l.hasClause(ClawClause.ACC) }? acc_clause[$l]
  | { !$l.hasClause(ClawClause.TARGET) }?      target_clause[$l]
  )*
;

// Possible permutation of clauses for the expand directive
expand_clauses[ClawPragma l]:
  (
    { !$l.hasClause(ClawClause.FUSION) }?      fusion_clause[$l]
  | { !$l.hasClause(ClawClause.PARALLEL) }?    parallel_clause[$l]
  | { !$l.hasClause(ClawClause.ACC) }? acc_clause[$l]
  | { !$l.hasClause(ClawClause.INDUCTION) }?   induction_clause[$l]
  | { !$l.hasClause(ClawClause.TARGET) }?      target_clause[$l]
  )*
;

// Possible permutation of clauses for the kcache directive
kcache_clauses[ClawPragma l]
@init{
    List<Integer> i = new ArrayList<>();
}
:
  (
    { $l.getOffsets() == null }? offset_clause[i] { $l.setOffsets(i); }
  | { !$l.hasClause(ClawClause.INIT) }?     INIT { $l.setClause(ClawClause.INIT); }
  | { !$l.hasClause(ClawClause.PRIVATE) }?  private_clause[$l]
  | { !$l.hasClause(ClawClause.TARGET) }?   target_clause[$l]
  )*
  {
    if($l.getOffsets() == null) {
      $l.setOffsets(i); // Set default offset if not specified
    }
  }
;

// Possible permutation of clauses for the loop-hoist directive
loop_hoist_clauses[ClawPragma l]:
  (
    { !$l.hasClause(ClawClause.RESHAPE) }?     reshape_clause[$l]
  | { !$l.hasClause(ClawClause.INTERCHANGE) }? interchange_clause[$l]
  | { !$l.hasClause(ClawClause.FUSION) }?      fusion_clause[$l]
  | { !$l.hasClause(ClawClause.TARGET) }?      target_clause[$l]
  | { !$l.hasClause(ClawClause.CLEANUP) }?     cleanup_clause[$l]
  )*
;


/*----------------------------------------------------------------------------
 * LEXER RULES
 *----------------------------------------------------------------------------*/

// Start point
CLAW         : 'claw';

// CLAW Directives
ARRAY_TO_CALL    : 'call';
DEFINE           : 'define';
END              : 'end';
EXPAND           : 'expand';
IF_EXTRACT       : 'if-extract';
IGNORE           : 'ignore';
KCACHE           : 'kcache';
LOOP_EXTRACT     : 'loop-extract';
LOOP_FUSION      : 'loop-fusion';
LOOP_HOIST       : 'loop-hoist';
LOOP_INTERCHANGE : 'loop-interchange';
MODEL_DATA       : 'model-data';
PARALLELIZE      : 'parallelize';  // TODO to be removed
REMOVE           : 'remove';
SCA              : 'sca';
VERBATIM         : 'verbatim';
AUTOPORT         : 'autoport';

// CLAW Clauses
CLEANUP      : 'cleanup';
COLLAPSE     : 'collapse';
CONSTRAINT   : 'constraint';
COPY         : 'copy';
CREATE       : 'create';
DATA         : 'data';
DIMENSION    : 'dimension';
FORWARD      : 'forward';
FUSION       : 'fusion';
GROUP        : 'group';
INDUCTION    : 'induction';
INIT         : 'init';
INTERCHANGE  : 'interchange';
LAYOUT       : 'layout';
MAP          : 'map';
NOPROMOTE    : 'nopromote';
OFFSET       : 'offset';
OVER         : 'over';
PARALLEL     : 'parallel';
PRIVATE      : 'private';
RANGE        : 'range';
RESHAPE      : 'reshape';
ROUTINE      : 'routine';
SCALAR       : 'scalar';
TARGET       : 'target';
UPDATE       : 'update';
NODEP        : 'nodep';

// data copy/update clause keywords
IN           : 'in';
OUT          : 'out';

// Directive primitive clause
ACC          : 'acc';
OMP          : 'omp';

// Target for the target clause
CPU          : 'cpu';
GPU          : 'gpu';
MIC          : 'mic';

// Constraint clause value
DIRECT       : 'direct';
NONE         : 'none';

// Special elements
IDENTIFIER      : [a-zA-Z_$] [a-zA-Z_$0-9-]* ;
NUMBER          : (DIGIT)+ ;
fragment DIGIT  : [0-9] ;

// Skip whitspaces
WHITESPACE   : ( '\t' | ' ' | '\r' | '\n'| '\u000C' )+ { skip(); };

REMAINING : .*? { skip(); };
