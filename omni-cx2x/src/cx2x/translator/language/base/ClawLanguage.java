/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */

package cx2x.translator.language.base;

import cx2x.translator.language.common.*;
import cx2x.translator.language.helper.accelerator.AcceleratorDirective;
import cx2x.translator.language.helper.accelerator.AcceleratorGenerator;
import cx2x.translator.language.helper.target.Target;
import cx2x.translator.language.parser.ClawErrorListener;
import cx2x.translator.language.parser.ClawLexer;
import cx2x.translator.language.parser.ClawParser;
import cx2x.xcodeml.exception.IllegalDirectiveException;
import cx2x.xcodeml.language.AnalyzedPragma;
import cx2x.xcodeml.xnode.Xnode;
import org.antlr.v4.runtime.ANTLRInputStream;
import org.antlr.v4.runtime.BailErrorStrategy;
import org.antlr.v4.runtime.CommonTokenStream;
import org.antlr.v4.runtime.misc.ParseCancellationException;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

/**
 * ClawLanguage class represent an analyzed pragma statement.
 *
 * @author clementval
 */
public class ClawLanguage extends AnalyzedPragma {

  private static final String PREFIX_CLAW = "claw";
  private static final String IGNORE = "ignore";

  private AcceleratorGenerator _generator;
  private Target _target;
  private ClawDirective _directive;

  // Clauses values
  private String _accClausesValue;
  private String _arrayName;
  private int _collapseClauseValue;
  private List<String> _dataValues;
  private String _groupClauseValue;
  private List<String> _fctCallParameters;
  private String _fctName;
  private List<String> _hoistInductionValues;
  private List<String> _indexesValues;
  private List<String> _inductionClauseValues;
  private List<ClawMapping> _mappingValues;
  private List<Integer> _offsetValues;
  private ClawRange _rangeValue;
  private List<ClawReshapeInfo> _reshapeInfos;
  private List<ClawDimension> _dimensions;
  private List<List<String>> _overValues;
  private List<List<String>> _overDataValues;
  private ClawDMD _copyClauseValue;
  private ClawDMD _updateClauseValue;
  private List<Target> _targetClauseValues;
  private ClawConstraint _constraintClauseValue;

  // Clauses flags
  private boolean _hasAccClause, _hasCollapseClause, _hasDataClause;
  private boolean _hasDimensionClause, _hasFusionClause, _hasGroupClause;
  private boolean _hasIndexesValue, _hasInductionClause, _hasInitClause;
  private boolean _hasInterchangeClause, _hasOverClause, _hasParallelClause;
  private boolean _hasPrivateClause, _hasReshapeClause, _hasForward;
  private boolean _hasOverDataClause, _hasCopyClause, _hasUpdateClause;
  private boolean _hasTargetClause, _hasConstraintClause;

  /**
   * Constructs an empty ClawLanguage section.
   * WARNING: This ctor should only be used by the parser.
   */
  public ClawLanguage() {
    resetVariables();
  }

  /**
   * Constructs an empty ClawLanguage object with an attached pragma. Used only
   * for transformation that are not CLAW related.
   *
   * @param pragma The pragma that is attached to the ClawLanguage object.
   */
  public ClawLanguage(Xnode pragma) {
    super(pragma);
    resetVariables();
  }

  /**
   * Check if the pragma statement starts with the claw keyword.
   *
   * @param pragma The raw pragma element object to check.
   * @return True if the statement starts with claw keyword. False otherwise.
   */
  public static boolean startsWithClaw(Xnode pragma) {
    return !(pragma == null || pragma.value() == null)
        && pragma.value().startsWith(PREFIX_CLAW);
  }

  /**
   * Analyze a raw string input and match it with the CLAW language definition.
   *
   * @param pragma    A raw pragma element object to be analyzed against the
   *                  CLAW language.
   * @param generator Accelerator directive generator.
   * @param target    Target that influences the code transformation.
   * @return A ClawLanguage object with the corresponding extracted information.
   * @throws IllegalDirectiveException If directive does not follow the CLAW
   *                                   language specification.
   */
  public static ClawLanguage analyze(Xnode pragma,
                                     AcceleratorGenerator generator,
                                     Target target)
      throws IllegalDirectiveException
  {
    ClawLanguage l =
        analyze(pragma.value(), pragma.lineNo(), generator, target);
    if(l != null) {
      l.attachPragma(pragma);
    }
    return l;
  }

  /**
   * Produce a "naked" pragma.
   * OMNI compiler keeps the claw prefix when a pragma is defined on several
   * lines using the continuation symbol '&'. In order to have a simpler
   * grammar, these multiple occurrences of the prefix are not taken into
   * account. Therefore, this method remove all the prefix and keeps only the
   * first one.
   *
   * @param rawPragma The original raw pragma statement straight from OMNI
   *                  compiler representation.
   * @return A naked pragma statement able to be analyzed by the CLAW parser.
   */
  private static String nakenize(String rawPragma) {
    return PREFIX_CLAW + " " +
        rawPragma.toLowerCase().replaceAll(PREFIX_CLAW, "");
  }

  /**
   * Analyze a raw string input and match it with the CLAW language definition.
   *
   * @param rawPragma A raw pragma statement to be analyzed against the CLAW
   *                  language.
   * @param lineno    Line number of the pragma statement.
   * @param generator Accelerator directive generator.
   * @param target    Target that influences the code transformation.
   * @return A ClawLanguage object with the corresponding extracted information.
   * @throws IllegalDirectiveException If directive does not follow the CLAW
   *                                   language specification.
   */
  private static ClawLanguage analyze(String rawPragma,
                                      int lineno,
                                      AcceleratorGenerator generator,
                                      Target target)
      throws IllegalDirectiveException
  {
    // Remove additional claw keyword
    rawPragma = nakenize(rawPragma);

    // Discard the ignored code after the claw ignore directive
    if(rawPragma.toLowerCase().contains(IGNORE)) {
      rawPragma = rawPragma.substring(0,
          rawPragma.toLowerCase().indexOf(IGNORE) + IGNORE.length());
    }

    // Instantiate the lexer with the raw string input
    ClawLexer lexer = new ClawLexer(new ANTLRInputStream(rawPragma));

    // Get a list of matched tokens
    CommonTokenStream tokens = new CommonTokenStream(lexer);

    // Pass the tokens to the parser
    ClawParser parser = new ClawParser(tokens);
    parser.setErrorHandler(new BailErrorStrategy());
    parser.removeErrorListeners();
    ClawErrorListener cel = new ClawErrorListener();
    parser.addErrorListener(cel);

    try {
      // Start the parser analysis from the "analyze" entry point
      ClawParser.AnalyzeContext ctx = parser.analyze();
      // Get the ClawLanguage object return by the parser after analysis.
      ctx.l.setAcceleratorGenerator(generator);
      ctx.l.setTarget(target);
      return ctx.l;
    } catch(ParseCancellationException pcex) {
      IllegalDirectiveException ex = cel.getLastError();
      if(ex != null) {
        throw ex;
      } else {
        throw new IllegalDirectiveException(rawPragma,
            "Unsupported construct", lineno, 0);
      }
    }
  }

  /**
   * Create an instance of ClawLanguage that correspond to a loop-fusion
   * directive. Used for dynamically created transformation.
   *
   * @param master Base object which initiate the creation of this instance.
   * @return An instance of ClawLanguage describing a loop-fusion with the
   * group, collapse clauses and the pragma from the master object.
   */
  public static ClawLanguage createLoopFusionLanguage(ClawLanguage master) {
    ClawLanguage l = new ClawLanguage();
    l.setDirective(ClawDirective.LOOP_FUSION);
    l.setGroupClause(master.getGroupValue());
    l.setCollapseClause(master.getCollapseValue());
    l.attachPragma(master.getPragma());
    return l;
  }

  /**
   * Create an instance of ClawLanguage that correspond to a loop-interchange
   * directive. Used for dynamically created transformation.
   *
   * @param master Base object which initiate the creation of this instance.
   * @param pragma Pragma statement located just before the first do stmt.
   * @return An instance of ClawLanguage describing a loop-interchange with the
   * indexes from the master object.
   */
  public static ClawLanguage createLoopInterchangeLanguage(ClawLanguage master,
                                                           Xnode pragma)
  {
    ClawLanguage l = new ClawLanguage();
    l.setDirective(ClawDirective.LOOP_INTERCHANGE);
    l.setIndexes(master.getIndexes());
    l.attachPragma(pragma);
    return l;
  }

  private void resetVariables() {
    // Clauses values members
    _accClausesValue = null;
    _arrayName = null;
    _collapseClauseValue = 0;
    _dataValues = null;
    _dimensions = null;
    _fctCallParameters = null;
    _fctName = null;
    _groupClauseValue = null;
    _hoistInductionValues = null;
    _indexesValues = null;
    _inductionClauseValues = null;
    _mappingValues = null;
    _offsetValues = null;
    _overValues = null;
    _overDataValues = null;
    _rangeValue = null;
    _reshapeInfos = null;
    _targetClauseValues = null;
    _constraintClauseValue = ClawConstraint.DIRECT;

    // Clauses flags members
    _hasAccClause = false;
    _hasCollapseClause = false;
    _hasCopyClause = false;
    _hasDimensionClause = false;
    _hasFusionClause = false;
    _hasForward = false;
    _hasGroupClause = false;
    _hasIndexesValue = false;
    _hasInductionClause = false;
    _hasInitClause = false;
    _hasInterchangeClause = false;
    _hasOverClause = false;
    _hasOverDataClause = false;
    _hasParallelClause = false;
    _hasPrivateClause = false;
    _hasReshapeClause = false;
    _hasUpdateClause = false;
    _hasTargetClause = false;
    _hasConstraintClause = false;

    // General members
    _directive = null;
    _generator = null;

    // super class members
    _pragma = null;

    // Data Movement Direction
    _copyClauseValue = null;
    _updateClauseValue = null;
  }

  /**
   * Check whether a group clause was specified.
   *
   * @return True if group clause was specified.
   */
  public boolean hasGroupClause() {
    return _hasGroupClause;
  }

  /**
   * Set the group name and hasGroupClause to true
   *
   * @param groupName The group name defined in the group clause.
   */
  public void setGroupClause(String groupName) {
    if(groupName != null) {
      _hasGroupClause = true;
      _groupClauseValue = groupName;
    }
  }

  /**
   * Get the group name defined in the group clause.
   *
   * @return The group name as a String value.
   */
  public String getGroupValue() {
    return _groupClauseValue;
  }

  /**
   * Check whether the collapse clause is used.
   *
   * @return True if the collapse clause if used.
   */
  public boolean hasCollapseClause() {
    return _hasCollapseClause;
  }

  /**
   * Set the collapse number and boolean flag.
   *
   * @param n Number of loop to be collapsed. Will be converted to integer.
   */
  public void setCollapseClause(String n) {
    setCollapseClause(Integer.parseInt(n));
  }

  // Loop interchange specific methods

  /**
   * Set the collapse number and boolean flag. Flag is enable if n is greater
   * than 1. Otherwise, collapse clause has no impact.
   *
   * @param n Number of loops to be collapsed.
   */
  private void setCollapseClause(int n) {
    if(n > 1) {
      _hasCollapseClause = true;
      _collapseClauseValue = n;
    }
  }

  /**
   * Get the collapse clause extracted value.
   *
   * @return An integer value.
   */
  public int getCollapseValue() {
    return _collapseClauseValue;
  }

  /**
   * Get the loop index list
   *
   * @return List of loop index
   */
  public List<String> getIndexes() {
    return _indexesValues;
  }

  // Loop extract specific methods

  /**
   * Set the list of interchange indexes.
   *
   * @param indexes List of indexes as string.
   */
  public void setIndexes(List<String> indexes) {
    _hasIndexesValue = true;
    _indexesValues = indexes;
  }

  /**
   * Check whether the interchange directive has indexes values.
   *
   * @return True if the directive has interchange value.
   */
  public boolean hasIndexes() {
    return _hasIndexesValue;
  }

  /**
   * Get the range extracted value.
   *
   * @return A ClawRange object.
   */
  public ClawRange getRange() {
    return _rangeValue;
  }

  /**
   * Set the range value.
   *
   * @param range A ClawRange object.
   */
  public void setRange(ClawRange range) {
    _rangeValue = range;
  }

  /**
   * Get the list of extracted ClawMapping objects.
   *
   * @return List of ClawMapping objects.
   */
  public List<ClawMapping> getMappings() {
    return _mappingValues;
  }

  /**
   * Set the ClawMapping list
   *
   * @param mappings A list of ClawMapping objects.
   */
  public void setMappings(List<ClawMapping> mappings) {
    _mappingValues = mappings;
  }

  /**
   * Enable the fusion clause for the current directive.
   */
  public void setFusionClause() {
    _hasFusionClause = true;
  }

  /**
   * Check whether the current directive has the fusion clause enabled.
   *
   * @return True if the fusion clause is enabled.
   */
  public boolean hasFusionClause() {
    return _hasFusionClause;
  }

  /**
   * Enable the parallel clause for the current directive.
   */
  public void setParallelClause() {
    _hasParallelClause = true;
  }

  /**
   * Check whether the current directive has the parallel clause enabled.
   *
   * @return True if the parallel clause is enabled.
   */
  public boolean hasParallelClause() {
    return _hasParallelClause;
  }

  /**
   * Check whether the current directive has the accelerator clause enabled.
   *
   * @return True if the accelerator clause is enabled.
   */
  public boolean hasAcceleratorClause() {
    return _hasAccClause;
  }

  /**
   * Get the accelerator clauses extracted from the accelerator clause.
   *
   * @return Accelerator clauses as a String.
   */
  public String getAcceleratorClauses() {
    return _accClausesValue;
  }

  /**
   * Enable the accelerator clause for the current directive and set the
   * extracted clauses.
   *
   * @param clauses Accelerator clauses extracted from the accelerator clause.
   */
  public void setAcceleratorClauses(String clauses) {
    _hasAccClause = true;
    _accClausesValue = clauses;
  }

  // loop hoist clauses

  /**
   * Get the list of offsets.
   *
   * @return List of offsets.
   */
  public List<Integer> getOffsets() {
    return _offsetValues;
  }

  /**
   * Set the offsets list extracted from the kcache directive.
   *
   * @param offsets A list of offsets.
   */
  public void setOffsets(List<Integer> offsets) {
    _offsetValues = offsets;
  }

  /**
   * Check whether the interchange clause is used.
   *
   * @return True if the interchange clause if used.
   */
  public boolean hasInterchangeClause() {
    return _hasInterchangeClause;
  }

  /**
   * Set the interchange clause as used.
   */
  public void setInterchangeClause() {
    _hasInterchangeClause = true;
  }


  // Directive generic method

  /**
   * Get the list of induction variables used in the hoist directive.
   *
   * @return A list of induction variable.
   */
  public List<String> getHoistInductionVars() {
    return _hoistInductionValues;
  }

  /**
   * Set the list of induction variables used in the loop-hoist directive.
   *
   * @param vars List of induction variable.
   */
  public void setHoistInductionVars(List<String> vars) {
    _hoistInductionValues = vars;
  }

  /**
   * Get the current directive of the language section.
   *
   * @return Value of the current directive.
   */
  public ClawDirective getDirective() {
    return _directive;
  }

  /**
   * Define the current directive of the language section.
   *
   * @param directive A value of the ClawDirective enumeration.
   */
  public void setDirective(ClawDirective directive) {
    _directive = directive;
  }

  /**
   * Enable the induction clause for the current directive and set the extracted
   * name value.
   *
   * @param names List of induction name extracted from the clause.
   */
  public void setInductionClause(List<String> names) {
    _hasInductionClause = true;
    _inductionClauseValues = names;
  }

  /**
   * Check whether the current directive has the induction clause enabled.
   *
   * @return True if the induction clause is enabled.
   */
  public boolean hasInductionClause() {
    return _hasInductionClause;
  }

  /**
   * Get the name value extracted from the induction clause.
   *
   * @return Induction name as a String.
   */
  public List<String> getInductionValues() {
    return _inductionClauseValues;
  }

  /**
   * Enable the data clause for the current directive and set the extracted
   * identifiers value.
   *
   * @param data List of identifiers extracted from the clause.
   */
  public void setDataClause(List<String> data) {
    _hasDataClause = true;
    _dataValues = data;
  }

  /**
   * Check whether the current directive has the induction clause enabled.
   *
   * @return True if the data clause is enabled.
   */
  public boolean hasDataClause() {
    return _hasDataClause;
  }

  /**
   * Get the identifier values extracted from the data clause.
   *
   * @return Identifier as a String.
   */
  public List<String> getDataClauseValues() {
    return _dataValues;
  }

  /**
   * Enable the over clause for the current directive and set the extracted
   * dimensions value.
   *
   * @param data List of dimension extracted from the clause.
   */
  public void setOverClause(List<String> data) {
    if(_overValues == null) {
      _hasOverClause = true;
      _overValues = new ArrayList<>();
    }
    _overValues.add(data);
  }

  /**
   * Check whether the current directive has the over clause enabled.
   *
   * @return True if the over clause is enabled.
   */
  public boolean hasOverClause() {
    return _hasOverClause;
  }

  /**
   * Get the dimensions values extracted from the over clause.
   *
   * @return Dimensions identifier or : as a String.
   */
  public List<List<String>> getOverClauseValues() {
    return _overValues;
  }

  /**
   * Enable the data over clause for the current directive.
   *
   * @param data List of array identifiers extracted from the clause.
   */
  public void setOverDataClause(List<String> data) {
    if(_overDataValues == null) {
      _hasOverDataClause = true;
      _overDataValues = new ArrayList<>();
    }
    _overDataValues.add(data);
  }

  /**
   * Check whether the current directive has the data over clause enabled.
   *
   * @return True if the data over clause is enabled.
   */
  public boolean hasOverDataClause() {
    return _hasOverDataClause;
  }

  /**
   * Get the data values extracted from the data over clause.
   *
   * @return Array identifier.
   */
  public List<List<String>> getOverDataClauseValues() {
    return _overDataValues;
  }

  /**
   * Check whether the init clause is used.
   *
   * @return True if the init clause is used.
   */
  public boolean hasInitClause() {
    return _hasInitClause;
  }

  /**
   * Set the init clause flag.
   */
  public void setInitClause() {
    _hasInitClause = true;
  }

  /**
   * Check whether the private clause is used.
   *
   * @return True if the private clause is used.
   */
  public boolean hasPrivateClause() {
    return _hasPrivateClause;
  }

  /**
   * Set the private clause flag.
   */
  public void setPrivateClause() {
    _hasPrivateClause = true;
  }

  /**
   * Get the list of parameters extracted from the call directive.
   *
   * @return List of parameters identifier as String value.
   */
  public List<String> getFctParams() {
    return _fctCallParameters;
  }

  /**
   * Set the list of parameters for the fct call of the "call" directive
   *
   * @param data List of identifiers extracted from the clause.
   */
  public void setFctParams(List<String> data) {
    _fctCallParameters = data;
  }

  /**
   * Get the array name extracted from the call directive.
   *
   * @return Array name from the call directive.
   */
  public String getArrayName() {
    return _arrayName;
  }

  /**
   * Set the array name value.
   *
   * @param value String value for the array name.
   */
  public void setArrayName(String value) {
    _arrayName = value;
  }

  /**
   * Get the fct name extracted from the call directive.
   *
   * @return Fct name from the call directive.
   */
  public String getFctName() {
    return _fctName;
  }

  /**
   * Set the function name value.
   *
   * @param value String value for the function name.
   */
  public void setFctName(String value) {
    _fctName = value;
  }

  /**
   * Get the reshape extracted information.
   *
   * @return List of ClawReshapeInfo objects containing the extracted
   * information from the reshape clause.
   */
  public List<ClawReshapeInfo> getReshapeClauseValues() {
    return _reshapeInfos;
  }

  /**
   * Set the reshape clause extracted information.
   *
   * @param infos List of ClawReshapeInfo objects containing the extracted
   *              information from the reshape clause.
   */
  public void setReshapeClauseValues(List<ClawReshapeInfo> infos) {
    _hasReshapeClause = true;
    _reshapeInfos = infos;
  }

  /**
   * Check whether the reshape clause is used.
   *
   * @return True if the reshape clause is used.
   */
  public boolean hasReshapeClause() {
    return _hasReshapeClause;
  }

  /**
   * Set the forward clause.
   */
  public void setForwardClause() {
    _hasForward = true;
  }

  /**
   * Check whether the forward clause is used.
   *
   * @return True if the forward clause is used.
   */
  public boolean hasForwardClause() {
    return _hasForward;
  }

  /**
   * Check whether the dimension clause is used.
   *
   * @return True if the dimension clause is used.
   */
  public boolean hasDimensionClause() {
    return _hasDimensionClause;
  }

  /**
   * Add a new dimension extracted from the directive.
   *
   * @param dimension ClawDimension object constructed from the value extracted
   *                  in the clause.
   */
  public void addDimension(ClawDimension dimension) {
    _hasDimensionClause = true;
    if(_dimensions == null) {
      _dimensions = new ArrayList<>();
    }
    _dimensions.add(dimension);
  }

  /**
   * Check whether the copy clause is used.
   *
   * @return True if the copy clause is used.
   */
  public boolean hasCopyClause() {
    return _hasCopyClause;
  }

  /**
   * Get the copy clause value.
   *
   * @return Copy clause value as a ClawDMD enum value.
   */
  public ClawDMD getCopyClauseValue() {
    return _copyClauseValue;
  }

  /**
   * Set the copy clause value and the copy clause usage flag to true.
   *
   * @param value New copy clause value.
   */
  public void setCopyClauseValue(ClawDMD value) {
    _hasCopyClause = true;
    _copyClauseValue = value;
  }

  /**
   * Check whether the update clause is used.
   *
   * @return True if the update clause is used.
   */
  public boolean hasUpdateClause() {
    return _hasUpdateClause;
  }

  /**
   * Get the update clause value.
   *
   * @return Update clause value as a ClawDMD enum value.
   */
  public ClawDMD getUpdateClauseValue() {
    return _updateClauseValue;
  }

  /**
   * Set the update clause value and the update clause usage flag to true.
   *
   * @param value New update clause value.
   */
  public void setUpdateClauseValue(ClawDMD value) {
    _hasUpdateClause = true;
    _updateClauseValue = value;
  }

  /**
   * Check whether the target clause is used.
   *
   * @return True if the target clause is used.
   */
  public boolean hasTargetClause() {
    return _hasTargetClause;
  }

  /**
   * Get the target clause value.
   *
   * @return Target clause value as a list of Target enum value.
   */
  public List<Target> getTargetClauseValues() {
    return _targetClauseValues;
  }

  /**
   * Set the target clause value and the update clause usage flag to true.
   *
   * @param values New target clause values.
   */
  public void setTargetClauseValue(List<Target> values) {
    _hasTargetClause = true;
    _targetClauseValues = values;
  }

  /**
   * Check whether the constraint clause is used.
   *
   * @return True if the constraint clause is used.
   */
  public boolean hasConstraintClause() {
    return _hasConstraintClause;
  }

  /**
   * Get the constraint clause value.
   *
   * @return Constraint clause value as an enum value.
   */
  public ClawConstraint getConstraintClauseValue() {
    return _constraintClauseValue;
  }

  /**
   * Set the constraint clause value and the update clause usage flag to true.
   *
   * @param value New constraint clause value.
   */
  public void setConstraintClauseValue(ClawConstraint value) {
    _hasConstraintClause = true;
    _constraintClauseValue = value;
  }

  /**
   * Get the dimensions extracted information.
   *
   * @return All dimensions extracted from the directive.
   */
  public List<ClawDimension> getDimensionValues() {
    return _dimensions;
  }

  /**
   * Get the dimensions extracted information in reverse order.
   *
   * @return All dimensions extracted from the directive in reverse order.
   */
  public List<ClawDimension> getDimensionValuesReversed() {
    List<ClawDimension> tmp = new ArrayList<>(_dimensions);
    Collections.reverse(tmp);
    return tmp;
  }

  /**
   * Attach the pragma related to this CLAW language analysis.
   *
   * @param pragma Raw pragma element object.
   */
  private void attachPragma(Xnode pragma) {
    _pragma = pragma;
  }

  /**
   * Get the accelerator generator for the current accelerator directive
   * language associated with the program.
   *
   * @return Associated accelerator directive generator.
   */
  public AcceleratorGenerator getAcceleratorGenerator() {
    return _generator;
  }

  /**
   * Set the accelerator directive generator for pragma generation
   *
   * @param generator The current accelerator directive generator.
   */
  private void setAcceleratorGenerator(AcceleratorGenerator generator) {
    _generator = generator;
  }

  /**
   * Get the associated target.
   *
   * @return Target.
   */
  public Target getTarget() {
    return _target;
  }

  /**
   * Set the target for code transformation.
   *
   * @param target A target value from the enumeration.
   */
  private void setTarget(Target target) {
    _target = target;
  }

  /**
   * Check if the current transformation target (defined by user) match with the
   * target defined in the clause.
   *
   * @return True if the targets matches.
   */
  public boolean isApplicableToCurrentTarget() {
    return _targetClauseValues == null
        || _targetClauseValues.size() == 0
        || _targetClauseValues.contains(_target);
  }

  /**
   * Get the current accelerator directive language target.
   *
   * @return Value of the AcceleratorDirective enumeration.
   */
  public AcceleratorDirective getDirectiveLanguage() {
    return (_generator != null) ? _generator.getDirectiveLanguage() :
        AcceleratorDirective.NONE;
  }


}
