/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */

package cx2x.translator.language;

import cx2x.translator.language.helper.accelerator.AcceleratorDirective;
import cx2x.translator.language.helper.accelerator.AcceleratorGenerator;
import cx2x.translator.language.helper.target.Target;
import cx2x.xcodeml.exception.IllegalDirectiveException;
import cx2x.xcodeml.language.AnalyzedPragma;
import cx2x.xcodeml.xelement.Xpragma;
import org.antlr.v4.runtime.ANTLRInputStream;
import org.antlr.v4.runtime.BailErrorStrategy;
import org.antlr.v4.runtime.CommonTokenStream;
import org.antlr.v4.runtime.misc.ParseCancellationException;

import java.util.ArrayList;
import java.util.List;

/**
 * ClawLanguage class represent an analyzed pragma statement.
 *
 * @author clementval
 */
public class ClawLanguage extends AnalyzedPragma {

  private static final String PREFIX_CLAW = "claw";

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
  private List<String> _overValues;

  // Clauses flags
  private boolean _hasAccClause, _hasCollapseClause, _hasDataClause;
  private boolean _hasDimensionClause, _hasFusionClause, _hasGroupClause;
  private boolean _hasIndexesValue, _hasInductionClause, _hasInitClause;
  private boolean _hasInterchangeClause, _hasOverClause, _hasParallelClause;
  private boolean _hasPrivateClause, _hasReshapeClause;

  /**
   * Constructs an empty ClawLanguage section.
   * WARNING: This ctor should only be used by the parser.
   */
  protected ClawLanguage(){
    resetVariables();
  }

  /**
   * Constructs an empty ClawLanguage object with an attached pragma. Used only
   * for transformation that are not CLAW related.
   * @param pragma The pragma that is attached to the ClawLanguage object.
   */
  public ClawLanguage(Xpragma pragma){
    super(pragma);
    resetVariables();
  }

  private void resetVariables(){
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
    _rangeValue = null;
    _reshapeInfos = null;

    // Clauses flags members
    _hasAccClause = false;
    _hasCollapseClause = false;
    _hasDimensionClause = false;
    _hasFusionClause = false;
    _hasGroupClause = false;
    _hasIndexesValue = false;
    _hasInductionClause = false;
    _hasInitClause = false;
    _hasInterchangeClause = false;
    _hasOverClause = false;
    _hasParallelClause = false;
    _hasPrivateClause = false;
    _hasReshapeClause = false;

    // General members
    _directive = null;
    _generator = null;

    // super class members
    _pragma = null;
  }

  /**
   * Check if the pragma statement starts with the claw keyword.
   * @param pragma The Xpragma object to check.
   * @return True if the statement starts with claw keyword. False otherwise.
   */
  public static boolean startsWithClaw(Xpragma pragma) {
    return !(pragma == null || pragma.getValue() == null)
        && pragma.getValue().startsWith(PREFIX_CLAW);
  }

  /**
   * Analyze a raw string input and match it with the CLAW language definition.
   * @param pragma    A Xpragma object to be analyzed against the CLAW language.
   * @param generator Accelerator directive generator.
   * @param target Target that influences the code transformation.
   * @return A ClawLanguage object with the corresponding extracted information.
   * @throws IllegalDirectiveException If directive does not follow the CLAW
   * language specification. 
   */
  public static ClawLanguage analyze(Xpragma pragma,
                                     AcceleratorGenerator generator,
                                     Target target)
      throws IllegalDirectiveException
  {

    /*
     * OMNI compiler keeps the claw prefix when a pragma is defined on several
     * lines using the continuation symbol '&'. In order to have a simpler
     * grammar, these multiple occurences of the prefix are not taken into
     * account. Therefore, this method remove all the prefix and keeps only the
     * first one.
     */
    String nakedPragma = pragma.getValue().toLowerCase().replaceAll(PREFIX_CLAW, "");
    nakedPragma = PREFIX_CLAW + " " + nakedPragma;

    // Instantiate the lexer with the raw string input
    ClawLexer lexer = new ClawLexer(new ANTLRInputStream(nakedPragma));

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
      ctx.l.attachPragma(pragma);
      ctx.l.setAcceleratorGenerator(generator);
      ctx.l.setTarget(target);
      return ctx.l;
    } catch(ParseCancellationException pcex){
      IllegalDirectiveException ex = cel.getLastError();
      if(ex != null){
        throw ex;
      } else {
        throw new IllegalDirectiveException("", "", 0, 0); // TODO
      }
    }
  }

  /**
   * Check whether a group clause was specified.
   * @return True if group clause was specified.
   */
  public boolean hasGroupClause(){
    return _hasGroupClause;
  }

  /**
   * Set the group name and hasGroupClause to true
   * @param groupName The group name defined in the group clause.
   */
  void setGroupClause(String groupName){
    if(groupName != null) {
      _hasGroupClause = true;
      _groupClauseValue = groupName;
    }
  }

  /**
   * Get the group name defined in the group clause.
   * @return The group name as a String value.
   */
  public String getGroupValue(){
    return _groupClauseValue;
  }


  /**
   * Check whether the collapse clause is used.
   * @return True if the collapse clause if used.
   */
  public boolean hasCollapseClause(){
    return _hasCollapseClause;
  }

  /**
   * Set the collapse number and boolean flag.
   * @param n Number of loop to be collapsed. Will be converted to integer.
   */
  void setCollapseClause(String n){
    _hasCollapseClause = true;
    _collapseClauseValue = Integer.parseInt(n);
  }

  /**
   * Set the collapse number and boolean flag. Flag is enable if n is greater
   * than 1. Otherwise, collapse clause has no impact.
   * @param n Number of loops to be collapsed.
   */
  void setCollapseClause(int n){
    if(n > 1) {
      _hasCollapseClause = true;
      _collapseClauseValue = n;
    }
  }

  /**
   * Get the collapse clause extracted value.
   * @return An interger value.
   */
  public int getCollapseValue(){
    return _collapseClauseValue;
  }

  // Loop interchange specific methods

  /**
   * Set the list of interhcnage indexes.
   * @param indexes List of indexes as string.
   */
  void setIndexes(List<String> indexes){
    _hasIndexesValue = true;
    _indexesValues = indexes;
  }

  /**
   * Get the loop index list
   * @return List of loop index
   */
  public List<String> getIndexes(){
    return _indexesValues;
  }

  /**
   * Check whether the interchange directive has indexes values.
   * @return True if the directive has interchange value.
   */
  public boolean hasIndexes(){
    return _hasIndexesValue;
  }

  // Loop extract specific methods

  /**
   * Set the range value.
   * @param range A ClawRange object.
   */
  protected void setRange(ClawRange range){
    _rangeValue = range;
  }

  /**
   * Get the range extracted value.
   * @return A ClawRange object.
   */
  public ClawRange getRange(){
    return _rangeValue;
  }

  /**
   * Set the ClawMapping list
   * @param mappings A list of ClawMapping objects.
   */
  void setMappings(List<ClawMapping> mappings){
    _mappingValues = mappings;
  }

  /**
   * Get the list of extracted ClawMapping objects.
   * @return List of ClawMapping objects.
   */
  public List<ClawMapping> getMappings(){
    return _mappingValues;
  }

  /**
   * Enable the fusion clause for the current directive.
   */
  void setFusionClause(){
    _hasFusionClause = true;
  }

  /**
   * Check whether the current directive has the fusion clause enabled.
   * @return True if the fusion clause is enabled.
   */
  public boolean hasFusionClause(){
    return _hasFusionClause;
  }

  /**
   * Enable the parallel clause for the current directive.
   */
  void setParallelClause(){
    _hasParallelClause = true;
  }

  /**
   * Check whether the current directive has the parallel clause enabled.
   * @return True if the parallel clause is enabled.
   */
  public boolean hasParallelClause(){
    return _hasParallelClause;
  }

  /**
   * Enable the accelerator clause for the current directive and set the
   * extracted clauses.
   * @param clauses Accelerator clauses extracted from the accelerator clause.
   */
  void setAcceleratorClauses(String clauses){
    _hasAccClause = true;
    _accClausesValue = clauses;
  }

  /**
   * Check whether the current directive has the accelerator clause enabled.
   * @return True if the accelerator clause is enabled.
   */
  public boolean hasAcceleratorClause(){
    return _hasAccClause;
  }

  /**
   * Get the accelerator clauses extracted from the accelerator clause.
   * @return Accelerator clauses as a String.
   */
  public String getAcceleratorClauses(){
    return _accClausesValue;
  }

  /**
   * Set the offsets list extracted from the kcache directive.
   * @param offsets A list of offsets.
   */
  void setOffsets(List<Integer> offsets){
    _offsetValues = offsets;
  }

  /**
   * Get the list of offsets.
   * @return List of offsets.
   */
  public List<Integer> getOffsets(){
    return _offsetValues;
  }

  // loop hoist clauses

  /**
   * Check whether the interchange clause is used.
   * @return True if the interchange clause if used.
   */
  public boolean hasInterchangeClause(){
    return _hasInterchangeClause;
  }

  /**
   * Set the interchange clause as used.
   */
  void setInterchangeClause(){
    _hasInterchangeClause = true;
  }

  /**
   * Set the list of induction variables used in the loop-hoist directive.
   * @param vars List of induction variable.
   */
  void setHoistInductionVars(List<String> vars){
    _hoistInductionValues = vars;
  }

  /**
   * Get the list of induction variables used in the hoist directive.
   * @return A list of induction variable.
   */
  public List<String> getHoistInductionVars(){
    return _hoistInductionValues;
  }


  // Directive generic method

  /**
   * Define the current directive of the language section.
   * @param directive A value of the ClawDirective enumeration.
   */
  public void setDirective(ClawDirective directive){
    _directive = directive;
  }

  /**
   * Get the current directive of the language section.
   * @return Value of the current directive.
   */
  public ClawDirective getDirective(){
    return _directive;
  }

  /**
   * Enable the induction clause for the current directive and set the extracted
   * name value.
   * @param names List of induction name extracted from the clause.
   */
  void setInductionClause(List<String> names){
    _hasInductionClause = true;
    _inductionClauseValues = names;
  }

  /**
   * Check whether the current directive has the induction clause enabled.
   * @return True if the induction clause is enabled.
   */
  public boolean hasInductionClause(){
    return _hasInductionClause;
  }

  /**
   * Get the name value extracted from the induction clause.
   * @return Induction name as a String.
   */
  public List<String> getInductionValues(){
    return _inductionClauseValues;
  }



  /**
   * Enable the data clause for the current directive and set the extracted
   * identifiers value.
   * @param data List of identifiers extracted from the clause.
   */
  void setDataClause(List<String> data){
    _hasDataClause = true;
    _dataValues = data;
  }

  /**
   * Check whether the current directive has the induction clause enabled.
   * @return True if the data clause is enabled.
   */
  public boolean hasDataClause(){
    return _hasDataClause;
  }

  /**
   * Get the identifier values extracted from the data clause.
   * @return Identifier as a String.
   */
  public List<String> getDataClauseValues(){
    return _dataValues;
  }

  /**
   * Enable the over clause for the current directive and set the extracted
   * dimensions value.
   * @param data List of dimension extracted from the clause.
   */
  void setOverClause(List<String> data){
    _hasOverClause = true;
    _overValues = data;
  }

  /**
   * Check whether the current directive has the over clause enabled.
   * @return True if the over clause is enabled.
   */
  public boolean hasOverClause(){
    return _hasOverClause;
  }

  /**
   * Get the dimensions values extracted from the over clause.
   * @return Dimensions identifier or : as a String.
   */
  public List<String> getOverClauseValues(){
    return _overValues;
  }

  /**
   * Check whether the init clause is used.
   * @return True if the init clause is used.
   */
  public boolean hasInitClause(){
    return _hasInitClause;
  }

  /**
   * Set the init clause flag.
   */
  void setInitClause(){
    _hasInitClause = true;
  }

  /**
   * Check whether the private clause is used.
   * @return True if the private clause is used.
   */
  public boolean hasPrivateClause(){
    return _hasPrivateClause;
  }

  /**
   * Set the private clause flag.
   */
  void setPrivateClause(){
    _hasPrivateClause = true;
  }


  /**
   * Set the list of parameters for the fct call of the "call" directive
   * @param data List of identifiers extracted from the clause.
   */
  void setFctParams(List<String> data){
    _fctCallParameters = data;
  }

  /**
   * Get the list of parameters extracted from the call directive.
   * @return List of parameters identifier as String value.
   */
  public List<String> getFctParams(){
    return _fctCallParameters;
  }

  /**
   * Set the array name value.
   * @param value String value for the array name.
   */
  void setArrayName(String value){
    _arrayName = value;
  }

  /**
   * Get the array name extracted from the call directive.
   * @return Array name from the call directive.
   */
  public String getArrayName(){
    return _arrayName;
  }

  /**
   * Set the function name value.
   * @param value String value for the function name.
   */
  void setFctName(String value){
    _fctName = value;
  }

  /**
   * Get the fct name extracted from the call directive.
   * @return Fct name from the call directive.
   */
  public String getFctName(){
    return _fctName;
  }

  /**
   * Set the reshape clause extracted information.
   * @param infos List of ClawReshapeInfo objects containing the extracted
   * information from the reshape clause.
   */
  void setReshapeClauseValues(List<ClawReshapeInfo> infos){
    _hasReshapeClause = true;
    _reshapeInfos = infos;
  }

  /**
   * Get the reshape extracted information.
   * @return List of ClawReshapeInfo objects containing the extracted
   * information from the reshape clause.
   */
  public List<ClawReshapeInfo> getReshapeClauseValues(){
    return _reshapeInfos;
  }

  /**
   * Check whether the reshape clause is used.
   * @return True if the reshape clause is used.
   */
  public boolean hasReshapeClause(){
    return _hasReshapeClause;
  }

  /**
   * Check whether the dimesion clause is used.
   * @return True if the dimesion clause is used.
   */
  public boolean hasDimensionClause(){
    return _hasDimensionClause;
  }

  /**
   * Add a new dimension extracted from the directive.
   * @param dimension ClawDimension object constructed from the value extracted
   *                  in the clause.
   */
  void addDimension(ClawDimension dimension){
    _hasDimensionClause = true;
    if(_dimensions == null){
      _dimensions = new ArrayList<>();
    }
    _dimensions.add(dimension);
  }

  /**
   * Get the dimensions extracted information.
   * @return All dimensions extracted from the directive.
   */
  public List<ClawDimension> getDimesionValues(){
    return _dimensions;
  }

  /**
   * Attach the pragma related to this CLAW language analysis.
   * @param pragma Xpragma object.
   */
  private void attachPragma(Xpragma pragma){
    _pragma = pragma;
  }

  /**
   * Set the accelerator directive generator for pragma generation
   * @param generator The current accelerator directive generator.
   */
  private void setAcceleratorGenerator(AcceleratorGenerator generator){
    _generator = generator;
  }

  /**
   * Get the accelerator generator for the current accelerator directive
   * language associated with the program.
   * @return Associated accelerator directive generator.
   */
  public AcceleratorGenerator getAcceleratorGenerator(){
    return _generator;
  }

  /**
   * Set the target for code transformation.
   * @param target A target value from the enumeration.
   */
  private void setTarget(Target target){
    _target = target;
  }

  /**
   * Get the associated target.
   * @return Target.
   */
  public Target getTarget(){
    return _target;
  }

  /**
   * Get the current accelerator directive language target.
   * @return Value of the AcceleratorDirective enumeration.
   */
  public AcceleratorDirective getDirectiveLanguage(){
    return (_generator != null) ? _generator.getDirectiveLanguage() :
        AcceleratorDirective.NONE;
  }

  /**
   * Create an instance of ClawLanguage that correspond to a loop-fusion
   * directive. Used for dynamically created transformation.
   * @param master Base object which initiate the creation of this instance.
   * @return An instance of ClawLanguage describing a loop-fusion with the
   * group, collapse clauses and the pragma from the master object.
   */
  public static ClawLanguage createLoopFusionLanguage(ClawLanguage master){
    ClawLanguage l = new ClawLanguage();
    l.setDirective(ClawDirective.LOOP_FUSION);
    l.setGroupClause(master.getGroupValue());
    l.setCollapseClause(master.getCollapseValue());
    l.attachPragma(master.getPragma());
    return l;
  }

  /**
   * Create an instance of ClawLanguage that correspond to a loop-fusion
   * directive. Used for dynamically created transformation.
   * @param base     Pragma that triggered the transformation.
   * @param group    Group clause value.
   * @param collapse Collapse clause value.
   * @return An instance of ClawLanguage describing a loop-fusion with the
   * group, collapse clauses and the pragma from the master object.
   */
  public static ClawLanguage createLoopFusionLanguage(Xpragma base,
                                                      String group,
                                                      int collapse)
  {
    ClawLanguage l = new ClawLanguage();
    l.setDirective(ClawDirective.LOOP_FUSION);
    l.setGroupClause(group);
    l.setCollapseClause(collapse);
    l.attachPragma(base);
    return l;
  }

  /**
   * Create an instance of ClawLanguage that correspond to a loop-interchange
   * directive. Used for dynamically created transformation.
   * @param master Base object which initiate the creation of this instance.
   * @param pragma Pragma statement located just before the first do stmt.
   * @return An instance of ClawLanguage describing a loop-interchange with the
   * indexes from the master object.
   */
  public static ClawLanguage createLoopInterchangeLanguage(ClawLanguage master,
                                                           Xpragma pragma)
  {
    ClawLanguage l = new ClawLanguage();
    l.setDirective(ClawDirective.LOOP_INTERCHANGE);
    l.setIndexes(master.getIndexes());
    l.attachPragma(pragma);
    return l;
  }


}
