/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */
package claw.wani.language;

import claw.shenron.translator.AnalyzedPragma;
import claw.tatsu.common.CompilerDirective;
import claw.tatsu.common.Context;
import claw.tatsu.common.Target;
import claw.tatsu.directive.common.DataMovement;
import claw.tatsu.xcodeml.abstraction.DimensionDefinition;
import claw.tatsu.xcodeml.abstraction.InsertionPosition;
import claw.tatsu.xcodeml.abstraction.ReshapeInfo;
import claw.tatsu.xcodeml.exception.IllegalDirectiveException;
import claw.tatsu.xcodeml.xnode.common.Xnode;
import claw.wani.language.parser.ClawLexer;
import claw.wani.language.parser.ClawParser;
import claw.wani.x2t.configuration.Configuration;
import claw.wani.x2t.configuration.ModelConfig;
import org.antlr.v4.runtime.*;
import org.antlr.v4.runtime.InputMismatchException;
import org.antlr.v4.runtime.misc.IntervalSet;
import org.antlr.v4.runtime.misc.ParseCancellationException;

import java.util.*;

/**
 * ClawPragma class represent an analyzed pragma statement.
 *
 * @author clementval
 */
public class ClawPragma extends AnalyzedPragma {

  private static final String PREFIX_CLAW = "claw";
  private static final String IGNORE = "ignore";

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
  private List<ReshapeInfo> _reshapeInfos;
  //private List<List<String>> _overDataValues;
  private Set<String> _overDataValues;
  private List<String> _scalarValues;
  private DataMovement _copyClauseValue;
  private DataMovement _updateClauseValue;
  private List<Target> _targetClauseValues;
  private ClawConstraint _constraintClauseValue;
  private CompilerDirective _cleanupClauseValue;
  private String _layoutValue;
  private ModelConfig _localModelConfig;
  private List<String> _errors = new ArrayList<>();

  // Clauses flags
  private boolean _hasAccClause;
  private boolean _hasCollapseClause;
  private boolean _hasDataClause;
  private boolean _hasDataOverClause;
  private boolean _hasDimensionClause;
  private boolean _hasFusionClause;
  private boolean _hasGroupClause;
  private boolean _hasIndexesValue;
  private boolean _hasInductionClause;
  private boolean _hasInitClause;
  private boolean _hasInterchangeClause;
  private boolean _hasParallelClause;
  private boolean _hasPrivateClause;
  private boolean _hasReshapeClause;
  private boolean _hasForward;
  private boolean _hasCopyClause;
  private boolean _hasUpdateClause;
  private boolean _hasTargetClause;
  private boolean _hasConstraintClause;
  private boolean _hasScalarClause;
  private boolean _hasCreateClause;
  private boolean _hasCleanupClause;
  private boolean _hasLayoutClause;

  private boolean _scaModelConfig;

  /**
   * Constructs an empty ClawPragma section.
   * WARNING: This ctor should only be used by the parser.
   */
  public ClawPragma() {
    resetVariables();
  }

  /**
   * Constructs an empty ClawPragma object with an attached pragma. Used only
   * for transformation that are not CLAW related.
   *
   * @param pragma The pragma that is attached to the ClawPragma object.
   */
  public ClawPragma(Xnode pragma) {
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
   * @param pragma A raw pragma element object to be analyzed against the
   *               CLAW language.
   * @return A ClawPragma object with the corresponding extracted information.
   * @throws IllegalDirectiveException If directive does not follow the CLAW
   *                                   language specification.
   */
  public static ClawPragma analyze(Xnode pragma)
      throws IllegalDirectiveException
  {
    ClawPragma l =
        analyze(pragma.value(), pragma.lineNo());
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
   * @return A ClawPragma object with the corresponding extracted information.
   * @throws IllegalDirectiveException If directive does not follow the CLAW
   *                                   language specification.
   */
  private static ClawPragma analyze(String rawPragma, int lineno)
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
    ClawLexer lexer = new ClawLexer(CharStreams.fromString(rawPragma));

    // Get a list of matched tokens
    CommonTokenStream tokens = new CommonTokenStream(lexer);

    // Pass the tokens to the parser
    ClawParser parser = new ClawParser(tokens);
    parser.setErrorHandler(new BailErrorStrategy());
    parser.removeErrorListeners();

    try {
      // Start the parser analysis from the "analyze" entry point
      ClawParser.AnalyzeContext ctx = parser.analyze();
      // Get the ClawPragma object return by the parser after analysis.
      return ctx.l;
    } catch(ParseCancellationException pcex) {
      if(pcex.getCause() instanceof InputMismatchException) {
        InputMismatchException imex = (InputMismatchException) pcex.getCause();
        throw new IllegalDirectiveException(
            getTokens(imex.getExpectedTokens(), parser), lineno,
            imex.getOffendingToken().getCharPositionInLine());
      } else if(pcex.getCause() instanceof NoViableAltException) {
        NoViableAltException nvex = (NoViableAltException) pcex.getCause();
        throw new IllegalDirectiveException(nvex.getOffendingToken().getText(),
            getTokens(nvex.getExpectedTokens(), parser), lineno,
            nvex.getOffendingToken().getCharPositionInLine());
      }
      throw new IllegalDirectiveException(rawPragma,
          "Unsupported construct", lineno, 0);
    }
  }

  /**
   * Create an instance of ClawPragma that correspond to a loop-fusion
   * directive. Used for dynamically created transformation.
   *
   * @param master Base object which initiate the creation of this instance.
   * @return An instance of ClawPragma describing a loop-fusion with the
   * group, collapse clauses and the pragma from the master object.
   */
  public static ClawPragma createLoopFusionLanguage(ClawPragma master) {
    ClawPragma l = new ClawPragma();
    l.setDirective(ClawDirective.LOOP_FUSION);
    if(master.hasGroupClause()) {
      l.setGroupClause(master.getGroupValue());
    }
    if(master.hasCollapseClause()) {
      l.setCollapseClause(master.getCollapseValue());
    }
    if(master.hasConstraintClause()) {
      l.setConstraintClauseValue(master.getConstraintClauseValue());
    }
    l.attachPragma(master.getPragma());
    return l;
  }

  /**
   * Create an instance of ClawPragma that correspond to a loop-interchange
   * directive. Used for dynamically created transformation.
   *
   * @param master Base object which initiate the creation of this instance.
   * @param pragma Pragma statement located just before the first do stmt.
   * @return An instance of ClawPragma describing a loop-interchange with the
   * indexes from the master object.
   */
  public static ClawPragma createLoopInterchangeLanguage(ClawPragma master,
                                                         Xnode pragma)
  {
    ClawPragma l = new ClawPragma();
    l.setDirective(ClawDirective.LOOP_INTERCHANGE);
    l.setIndexes(master.getIndexes());
    l.attachPragma(pragma);
    return l;
  }

  /**
   * Get a readable list of token found in an IntervalSet.
   *
   * @param set    Set of tokens to be found.
   * @param parser Current parser instance.
   * @return List of human readable tokens.
   */
  private static List<String> getTokens(IntervalSet set, ClawParser parser) {
    List<String> tokens = new ArrayList<>();
    for(int tokenId : set.toList()) {
      if(parser.getVocabulary().getLiteralName(tokenId) == null) {
        tokens.add(parser.getVocabulary().getDisplayName(tokenId));
      } else {
        tokens.add(parser.getVocabulary().getLiteralName(tokenId));
      }
    }
    return tokens;
  }

  private void resetVariables() {
    // Clauses values members
    _accClausesValue = null;
    _arrayName = null;
    _collapseClauseValue = 1;
    _dataValues = null;
    _fctCallParameters = null;
    _fctName = null;
    _groupClauseValue = null;
    _hoistInductionValues = null;
    _indexesValues = null;
    _inductionClauseValues = null;
    _mappingValues = null;
    _offsetValues = null;
    _overDataValues = null;
    _rangeValue = null;
    _reshapeInfos = null;
    _targetClauseValues = null;
    _constraintClauseValue = ClawConstraint.DIRECT;
    _cleanupClauseValue = CompilerDirective.NONE;
    _layoutValue = null;

    // Clauses flags members
    _hasAccClause = false;
    _hasCollapseClause = false;
    _hasCopyClause = false;
    _hasDataClause = false;
    _hasDataOverClause = false;
    _hasDimensionClause = false;
    _hasFusionClause = false;
    _hasForward = false;
    _hasGroupClause = false;
    _hasIndexesValue = false;
    _hasInductionClause = false;
    _hasInitClause = false;
    _hasInterchangeClause = false;
    _hasParallelClause = false;
    _hasPrivateClause = false;
    _hasReshapeClause = false;
    _hasUpdateClause = false;
    _hasTargetClause = false;
    _hasConstraintClause = false;
    _hasScalarClause = false;
    _hasCreateClause = false;
    _hasCleanupClause = false;
    _hasLayoutClause = false;

    // General members
    _directive = null;

    _scaModelConfig = false;
    _localModelConfig = new ModelConfig();

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
   * @return An integer value. Default is 1.
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
   * Check whether the current directive has the directive clause enabled.
   *
   * @return True if the directive clause is enabled.
   */
  public boolean hasAcceleratorClause() {
    return _hasAccClause;
  }

  /**
   * Get the directive clauses extracted from the directive clause.
   *
   * @return Accelerator clauses as a String.
   */
  public String getAcceleratorClauses() {
    return _accClausesValue;
  }

  /**
   * Enable the directive clause for the current directive and set the
   * extracted clauses.
   *
   * @param clauses Accelerator clauses extracted from the directive clause.
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
   * Check whether the current directive has the induction clause enabled.
   *
   * @return True if the data clause is enabled.
   */
  public boolean hasDataClause() {
    return _hasDataClause;
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
   * Get the identifier values extracted from the data clause.
   *
   * @return Identifier as a String.
   */
  public List<String> getDataClauseValues() {
    return _dataValues;
  }

  /**
   * Process the data / over clause from the SCA directive. The over clause is
   * turned into a layout and each variable presented in the data clause will be
   * assigned this layout.
   *
   * @param data List of variable used in the data clause.
   * @param over List of dimension used in the over clause.
   */
  public void processDataOverClauses(List<String> data, List<String> over) {
    _hasDataOverClause = true;
    if(_overDataValues == null) {
      _overDataValues = new HashSet<>();
    }
    _overDataValues.addAll(data);

    int baseDimOccurrence = getNbOfBaseDimensions(over);
    if(baseDimOccurrence == 0) {
      _errors.add("Over clause does not specify the position " +
          "of existing dimensions.");
      return;
    }

    List<DimensionDefinition> overLayout = generateLayoutFromOver(over);
    for(String d : data) {
      if(_localModelConfig.hasLayout(d)) {
        _errors.add(String.format(
            "Variable %s has already a layout from another over clause.", d));
        return;
      } else {
        _localModelConfig.putLayout(d, overLayout);
      }
    }
  }

  /**
   * Get the number of base dimension placeholders used in the over definition.
   *
   * @param over List of dimension defined in over clause.
   * @return Number of base dimension placeholders found in the list.
   */
  private int getNbOfBaseDimensions(List<String> over) {
    int baseDimOccurrence = 0;
    for(String d : over) {
      if(d.equals(DimensionDefinition.BASE_DIM)) {
        ++baseDimOccurrence;
      }
    }
    return baseDimOccurrence;
  }

  /**
   * Generate a layout from the over clause information.
   *
   * @param over Over clause as a list of String (dimension ids).
   * @return A list of dimension definition used as a layout.
   */
  private List<DimensionDefinition> generateLayoutFromOver(List<String> over) {
    boolean hasMiddleInsertion = getNbOfBaseDimensions(over) > 1;
    List<DimensionDefinition> overLayout = new ArrayList<>();
    InsertionPosition crt = InsertionPosition.BEFORE;

    for(String d : over) {
      if(d.equals(DimensionDefinition.BASE_DIM)) {
        if(hasMiddleInsertion && crt == InsertionPosition.BEFORE) {
          crt = InsertionPosition.IN_MIDDLE;
        } else if(crt == InsertionPosition.BEFORE) {
          crt = InsertionPosition.AFTER;
        } else if(crt == InsertionPosition.IN_MIDDLE) {
          crt = InsertionPosition.AFTER;
        }
      } else {

        if(_localModelConfig.hasDimension(d)) {
          DimensionDefinition newDimension
              = _localModelConfig.getDimension(d).copy();
          newDimension.setInsertionPosition(crt);
          overLayout.add(newDimension);
        } else {
          _errors.add(String.format("Dimension %s is not defined", d));
        }
      }
    }
    return overLayout;
  }

  /**
   * Get the correct layout for a specific field if one is defined. Default
   * layout otherwise.
   *
   * @param dataId Data identifier.
   * @return Layout for the given field id.
   */
  public List<DimensionDefinition> getLayoutForData(String dataId) {
    if(_scaModelConfig) {
      if(getLocalModelConfig().hasLayout(dataId)) {
        return getLocalModelConfig().getLayout(dataId);
      }
      return Configuration.get().getModelConfig().getDefaultLayout();
    } else {
      if(_localModelConfig.hasLayout(dataId)) {
        return _localModelConfig.getLayout(dataId);
      }
      return _localModelConfig.getDefaultLayout();
    }
  }

  /**
   * Return the default layout in reverse order.
   *
   * @return Reversed list of dimensions from the default layout.
   */
  public List<DimensionDefinition> getDefaultLayoutReversed() {
    List<DimensionDefinition> tmp = new ArrayList<>(getDefaultLayout());
    Collections.reverse(tmp);
    return tmp;
  }

  /**
   * Get the default layout dimensions from the local or global configuration.
   *
   * @return List of dimensions from the default layout.
   */
  public List<DimensionDefinition> getDefaultLayout() {
    if(_scaModelConfig) {
      return Configuration.get().getModelConfig().getDefaultLayout();
    } else {
      return _localModelConfig.getDefaultLayout();
    }
  }

  /**
   * Check whether the current directive has the data over clause enabled.
   *
   * @return True if the data over clause is enabled.
   */
  public boolean hasDataOverClause() {
    return _hasDataOverClause;
  }

  /**
   * Get the data values extracted from the data over clause.
   *
   * @return Array identifier.
   */
  public Set<String> getDataOverClauseValues() {
    return _overDataValues;
  }

  /**
   * Enable scalar clause for the current directive and stores the data.
   *
   * @param data List of identifier declared in the scalar clause.
   */
  public void setScalarClause(List<String> data) {
    _hasScalarClause = true;
    _scalarValues = data;
  }

  /**
   * Check whether the current directly has the scalar clause enabled.
   *
   * @return True if the scalar clause is enabled.
   */
  public boolean hasScalarClause() {
    return _hasScalarClause;
  }

  /**
   * Get the data clause values extracted from the scalar clause.
   *
   * @return List of identifier declared in the scalar clause.
   */
  public List<String> getScalarClauseValues() {
    return _scalarValues;
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
   * @return List of ReshapeInfo objects containing the extracted
   * information from the reshape clause.
   */
  public List<ReshapeInfo> getReshapeClauseValues() {
    return _reshapeInfos;
  }

  /**
   * Set the reshape clause extracted information.
   *
   * @param infos List of ReshapeInfo objects containing the extracted
   *              information from the reshape clause.
   */
  public void setReshapeClauseValues(List<ReshapeInfo> infos) {
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
   * @param dimension DimensionDefinition object constructed from the value
   *                  extracted in the clause.
   */
  public void addDimension(DimensionDefinition dimension) {
    _hasDimensionClause = true;
    _localModelConfig.putDimension(dimension);
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
   * @return Copy clause value as a DataMovement enum value.
   */
  public DataMovement getCopyClauseValue() {
    return _copyClauseValue;
  }

  /**
   * Set the copy clause value and the copy clause usage flag to true.
   *
   * @param value New copy clause value.
   */
  public void setCopyClauseValue(DataMovement value) {
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
   * @return Update clause value as a DataMovement enum value.
   */
  public DataMovement getUpdateClauseValue() {
    return _updateClauseValue;
  }

  /**
   * Set the update clause value and the update clause usage flag to true.
   *
   * @param value New update clause value.
   */
  public void setUpdateClauseValue(DataMovement value) {
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
   * Attach the pragma related to this CLAW language analysis.
   *
   * @param pragma Raw pragma element object.
   */
  private void attachPragma(Xnode pragma) {
    _pragma = pragma;
  }

  /**
   * Check if the current transformation target (defined by user) match with the
   * target defined in the clause.
   *
   * @return True if the targets matches.
   */
  public boolean isApplicableToCurrentTarget() {
    return _targetClauseValues == null
        || _targetClauseValues.isEmpty()
        || _targetClauseValues.contains(Context.get().getTarget());
  }

  /**
   * Check whether the create clause is used.
   *
   * @return True if the create clause is used.
   */
  public boolean hasCreateClause() {
    return _hasCreateClause;
  }

  /**
   * Enable the create clause.
   */
  public void setCreateClause() {
    _hasCreateClause = true;
  }

  /**
   * Check whether the cleanup clause is used.
   *
   * @return True if the cleanup clause is used.
   */
  public boolean hasCleanupClause() {
    return _hasCleanupClause;
  }

  /**
   * Get the cleanup clause value.
   *
   * @return Cleanup clause value. NONE means both OpenACC and OpenMP.
   */
  public CompilerDirective getCleanupClauseValue() {
    return _cleanupClauseValue;
  }

  /**
   * Set the cleanup clause value and the update clause usage flag to true.
   *
   * @param value New compiler directive clause value.
   */
  public void setCleanupClauseValue(CompilerDirective value) {
    _hasCleanupClause = true;
    _cleanupClauseValue = value;
  }

  /**
   * Check whether the layout clause is used.
   *
   * @return True if the layout clause is used.
   */
  public boolean hasLayoutClause() {
    return _hasLayoutClause;
  }

  /**
   * Get the layout clause value.
   *
   * @return Layout clause value.
   */
  public String getLayoutValue() {
    return _layoutValue;
  }

  /**
   * Set the layout clause value and the update clause usage flag to true.
   *
   * @param value New compiler directive clause value.
   */
  public void setLayoutClause(String value) {
    _hasLayoutClause = true;
    _layoutValue = value;
  }

  /**
   * Set the current directive as a SCA from model config.
   */
  public void setScaModelConfig() {
    _scaModelConfig = true;
  }

  /**
   * Check whether the current directive is a SCA using model config.
   *
   * @return True if the current directive is a SCA using model config.
   */
  public boolean isScaModelConfig() {
    return _scaModelConfig;
  }

  /**
   * Return local configuration object. Used when dimensions and layouts are
   * defined directly in the pragma itself.
   *
   * @return Instance of ModelConfig object local to this pragma.
   */
  public ModelConfig getLocalModelConfig() {
    return _localModelConfig;
  }

  /**
   * Check whether any erros has been reported.
   *
   * @return True if any error reported. False otherwise.
   */
  public boolean hasErrors() {
    return !_errors.isEmpty();
  }

  /**
   * Get list of reported errors.
   *
   * @return List of errors.
   */
  public List<String> getErrors() {
    return _errors;
  }
}
