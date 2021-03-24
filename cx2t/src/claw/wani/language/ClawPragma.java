/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */
package claw.wani.language;

import java.util.ArrayList;
import java.util.Collections;
import java.util.EnumMap;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.antlr.v4.runtime.BailErrorStrategy;
import org.antlr.v4.runtime.CharStreams;
import org.antlr.v4.runtime.CommonTokenStream;
import org.antlr.v4.runtime.InputMismatchException;
import org.antlr.v4.runtime.NoViableAltException;
import org.antlr.v4.runtime.misc.IntervalSet;
import org.antlr.v4.runtime.misc.ParseCancellationException;

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

/**
 * ClawPragma class represent an analyzed pragma statement.
 *
 * @author clementval
 */
public class ClawPragma extends AnalyzedPragma
{

    private static final String PREFIX_CLAW = "claw";
    private static final String IGNORE = "ignore";

    private ClawDirective _directive;
    private final Set<ClawClause> _clauses = new HashSet<>();
    private final Map<ClawClause, String> _clauseStringValues = new EnumMap<>(ClawClause.class);
    private final Map<ClawClause, List<String>> _clauseListStringValues = new EnumMap<>(ClawClause.class);

    // Clauses values
    private int _collapseClauseValue;

    private List<ClawMapping> _mappingValues;
    private List<Integer> _offsetValues;
    private ClawRange _rangeValue;
    private List<ReshapeInfo> _reshapeInfos;
    private Set<String> _overDataValues;
    private DataMovement _copyClauseValue;
    private DataMovement _updateClauseValue;
    private List<Target> _targetClauseValues;
    private ClawConstraint _constraintClauseValue;
    private CompilerDirective _cleanupClauseValue;
    private Map<String, String> _metadataMap;

    // Model config information
    private ModelConfig _localModelConfig;
    private final List<String> _errors = new ArrayList<>();
    private boolean _scaModelConfig;

    /**
     * Constructs an empty ClawPragma section. WARNING: This ctor should only be
     * used by the parser.
     */
    public ClawPragma()
    {
        resetVariables();
    }

    /**
     * Constructs an empty ClawPragma object with an attached pragma. Used only for
     * transformation that are not CLAW related.
     *
     * @param pragma The pragma that is attached to the ClawPragma object.
     */
    public ClawPragma(Xnode pragma)
    {
        super(pragma);
        resetVariables();
    }

    /**
     * Check whether a clause is set.
     *
     * @param clause Clause to be checked.
     * @return True if the clause is set. False otherwise.
     */
    public boolean hasClause(ClawClause clause)
    {
        return _clauses.contains(clause);
    }

    /**
     * Set a clause.
     *
     * @param clause Clause to be set.
     */
    public void setClause(ClawClause clause)
    {
        if (clause != null)
        {
            _clauses.add(clause);
        }
    }

    /**
     * Set a clause and attach a String value to it.
     *
     * @param clause Clause to be set.
     * @param value  Value to be attached.
     */
    public void setValue(ClawClause clause, String value)
    {
        if (clause == null || value == null)
        {
            return;
        }
        _clauses.add(clause);
        _clauseStringValues.put(clause, value);
    }

    /**
     * Retrieve a String value attached to a clause.
     *
     * @param clause Clause for which the value is retrieved.
     * @return String value if clause is set. Null otherwise.
     */
    public String value(ClawClause clause)
    {
        return _clauseStringValues.getOrDefault(clause, null);
    }

    /**
     * Set a clause and attach a list of String values to it.
     *
     * @param clause Clause to be set.
     * @param values Values to be attached.
     */
    public void setValues(ClawClause clause, List<String> values)
    {
        if (clause == null || values == null)
        {
            return;
        }
        _clauses.add(clause);
        _clauseListStringValues.put(clause, values);
    }

    /**
     * Retrieve a list of String values attached to a clause.
     *
     * @param clause Clause for which the value is retrieved.
     * @return List of values if clause is set. Null otherwise.
     */
    public List<String> values(ClawClause clause)
    {
        return _clauseListStringValues.getOrDefault(clause, null);
    }

    /**
     * Check if the pragma statement starts with the claw keyword.
     *
     * @param pragma The raw pragma element object to check.
     * @return True if the statement starts with claw keyword. False otherwise.
     */
    public static boolean startsWithClaw(Xnode pragma)
    {
        return !(pragma == null || pragma.value() == null) && pragma.value().startsWith(PREFIX_CLAW);
    }

    /**
     * Analyze a raw string input and match it with the CLAW language definition.
     *
     * @param pragma A raw pragma element object to be analyzed against the CLAW
     *               language.
     * @return A ClawPragma object with the corresponding extracted information.
     * @throws IllegalDirectiveException If directive does not follow the CLAW
     *                                   language specification.
     */
    public static ClawPragma analyze(Xnode pragma) throws IllegalDirectiveException
    {
        ClawPragma l = analyze(pragma.value(), pragma.lineNo());
        if (l != null)
        {
            l.attachPragma(pragma);
        }
        return l;
    }

    /**
     * Produce a "naked" pragma. OMNI compiler keeps the claw prefix when a pragma
     * is defined on several lines using the continuation symbol '&'. In order to
     * have a simpler grammar, these multiple occurrences of the prefix are not
     * taken into account. Therefore, this method remove all the prefix and keeps
     * only the first one.
     *
     * @param rawPragma The original raw pragma statement straight from OMNI
     *                  compiler representation.
     * @return A naked pragma statement able to be analyzed by the CLAW parser.
     */
    private static String nakenize(String rawPragma)
    {
        return PREFIX_CLAW + " " + rawPragma.toLowerCase().replaceAll(PREFIX_CLAW, "");
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
    private static ClawPragma analyze(String rawPragma, int lineno) throws IllegalDirectiveException
    {
        // Remove additional claw keyword
        rawPragma = nakenize(rawPragma);

        // Discard the ignored code after the claw ignore directive
        if (rawPragma.toLowerCase().contains(IGNORE))
        {
            rawPragma = rawPragma.substring(0, rawPragma.toLowerCase().indexOf(IGNORE) + IGNORE.length());
        }

        // Instantiate the lexer with the raw string input
        ClawLexer lexer = new ClawLexer(CharStreams.fromString(rawPragma));

        // Get a list of matched tokens
        CommonTokenStream tokens = new CommonTokenStream(lexer);

        // Pass the tokens to the parser
        ClawParser parser = new ClawParser(tokens);
        parser.setErrorHandler(new BailErrorStrategy());
        parser.removeErrorListeners();

        try
        {
            // Start the parser analysis from the "analyze" entry point
            ClawParser.AnalyzeContext ctx = parser.analyze();
            // Get the ClawPragma object return by the parser after analysis.
            return ctx.l;
        } catch (ParseCancellationException pcex)
        {
            if (pcex.getCause() instanceof InputMismatchException)
            {
                InputMismatchException imex = (InputMismatchException) pcex.getCause();
                throw new IllegalDirectiveException(getTokens(imex.getExpectedTokens(), parser), lineno,
                        imex.getOffendingToken().getCharPositionInLine());
            } else if (pcex.getCause() instanceof NoViableAltException)
            {
                NoViableAltException nvex = (NoViableAltException) pcex.getCause();
                throw new IllegalDirectiveException(nvex.getOffendingToken().getText(),
                        getTokens(nvex.getExpectedTokens(), parser), lineno,
                        nvex.getOffendingToken().getCharPositionInLine());
            }
            throw new IllegalDirectiveException(rawPragma, "Unsupported construct", lineno, 0);
        }
    }

    /**
     * Create an instance of ClawPragma that correspond to a loop-fusion directive.
     * Used for dynamically created transformation.
     *
     * @param master Base object which initiate the creation of this instance.
     * @return An instance of ClawPragma describing a loop-fusion with the group,
     *         collapse clauses and the pragma from the master object.
     */
    public static ClawPragma createLoopFusionLanguage(ClawPragma master)
    {
        ClawPragma l = new ClawPragma();
        l.setDirective(ClawDirective.LOOP_FUSION);
        if (master.hasClause(ClawClause.GROUP))
        {
            l.setValue(ClawClause.GROUP, master.value(ClawClause.GROUP));
        }
        if (master.hasClause(ClawClause.COLLAPSE))
        {
            l.setCollapseClause(master.getCollapseValue());
        }
        if (master.hasClause(ClawClause.CONSTRAINT))
        {
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
     *         indexes from the master object.
     */
    public static ClawPragma createLoopInterchangeLanguage(ClawPragma master, Xnode pragma)
    {
        ClawPragma l = new ClawPragma();
        l.setDirective(ClawDirective.LOOP_INTERCHANGE);
        l.setValues(ClawClause.INTERCHANGE_INDEXES, master.values(ClawClause.INTERCHANGE_INDEXES));
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
    private static List<String> getTokens(IntervalSet set, ClawParser parser)
    {
        List<String> tokens = new ArrayList<>();
        for (int tokenId : set.toList())
        {
            if (parser.getVocabulary().getLiteralName(tokenId) == null)
            {
                tokens.add(parser.getVocabulary().getDisplayName(tokenId));
            } else
            {
                tokens.add(parser.getVocabulary().getLiteralName(tokenId));
            }
        }
        return tokens;
    }

    private void resetVariables()
    {
        // Clauses values members
        _collapseClauseValue = 1;
        _mappingValues = null;
        _offsetValues = null;
        _overDataValues = null;
        _rangeValue = null;
        _reshapeInfos = null;
        _targetClauseValues = null;
        _constraintClauseValue = ClawConstraint.DIRECT;
        _cleanupClauseValue = CompilerDirective.NONE;

        // Clauses flags members
        _clauses.clear();

        _clauseStringValues.clear();
        _clauseListStringValues.clear();

        // General members
        _directive = null;

        _scaModelConfig = false;
        _localModelConfig = new ModelConfig();

        // Data Movement Direction
        _copyClauseValue = null;
        _updateClauseValue = null;

        // Savepoint clause
        _metadataMap = new HashMap<>();
    }

    /**
     * Set the collapse number and boolean flag.
     *
     * @param n Number of loop to be collapsed. Will be converted to integer.
     */
    public void setCollapseClause(String n)
    {
        setCollapseClause(Integer.parseInt(n));
    }

    // Loop interchange specific methods

    /**
     * Set the collapse number and boolean flag. Flag is enable if n is greater than
     * 1. Otherwise, collapse clause has no impact.
     *
     * @param n Number of loops to be collapsed.
     */
    private void setCollapseClause(int n)
    {
        if (n > 1)
        {
            setClause(ClawClause.COLLAPSE);
            _collapseClauseValue = n;
        }
    }

    /**
     * Get the collapse clause extracted value.
     *
     * @return An integer value. Default is 1.
     */
    public int getCollapseValue()
    {
        return _collapseClauseValue;
    }

    // Loop extract specific methods

    /**
     * Get the range extracted value.
     *
     * @return A ClawRange object.
     */
    public ClawRange getRange()
    {
        return _rangeValue;
    }

    /**
     * Set the range value.
     *
     * @param range A ClawRange object.
     */
    public void setRange(ClawRange range)
    {
        _rangeValue = range;
    }

    /**
     * Get the list of extracted ClawMapping objects.
     *
     * @return List of ClawMapping objects.
     */
    public List<ClawMapping> getMappings()
    {
        return _mappingValues;
    }

    /**
     * Set the ClawMapping list
     *
     * @param mappings A list of ClawMapping objects.
     */
    public void setMappings(List<ClawMapping> mappings)
    {
        _mappingValues = mappings;
    }

    // loop hoist clauses

    /**
     * Get the list of offsets.
     *
     * @return List of offsets.
     */
    public List<Integer> getOffsets()
    {
        return _offsetValues;
    }

    /**
     * Set the offsets list extracted from the kcache directive.
     *
     * @param offsets A list of offsets.
     */
    public void setOffsets(List<Integer> offsets)
    {
        _offsetValues = offsets;
    }

    // Directive generic method

    /**
     * Get the current directive of the language section.
     *
     * @return Value of the current directive.
     */
    public ClawDirective getDirective()
    {
        return _directive;
    }

    /**
     * Define the current directive of the language section.
     *
     * @param directive A value of the ClawDirective enumeration.
     */
    public void setDirective(ClawDirective directive)
    {
        _directive = directive;
    }

    /**
     * Process the data / over clause from the SCA directive. The over clause is
     * turned into a layout and each variable presented in the data clause will be
     * assigned this layout.
     *
     * @param data List of variable used in the data clause.
     * @param over List of dimension used in the over clause.
     */
    public void processDataOverClauses(List<String> data, List<String> over)
    {
        setClause(ClawClause.DATA_OVER);
        if (_overDataValues == null)
        {
            _overDataValues = new HashSet<>();
        }
        _overDataValues.addAll(data);

        int baseDimOccurrence = getNbOfBaseDimensions(over);
        if (baseDimOccurrence == 0)
        {
            _errors.add("Over clause does not specify the position " + "of existing dimensions.");
            return;
        }

        List<DimensionDefinition> overLayout = generateLayoutFromOver(over);
        for (String d : data)
        {
            if (_localModelConfig.hasLayout(d))
            {
                _errors.add(String.format("Variable %s has already a layout from another over clause.", d));
                return;
            } else
            {
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
    private int getNbOfBaseDimensions(List<String> over)
    {
        int baseDimOccurrence = 0;
        for (String d : over)
        {
            if (d.equals(DimensionDefinition.BASE_DIM))
            {
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
    private List<DimensionDefinition> generateLayoutFromOver(List<String> over)
    {
        boolean hasMiddleInsertion = getNbOfBaseDimensions(over) > 1;
        List<DimensionDefinition> overLayout = new ArrayList<>();
        InsertionPosition crt = InsertionPosition.BEFORE;

        for (String d : over)
        {
            if (d.equals(DimensionDefinition.BASE_DIM))
            {
                crt = crt.getNext(hasMiddleInsertion);
            } else
            {

                if (_localModelConfig.hasDimension(d))
                {
                    DimensionDefinition newDimension = _localModelConfig.getDimension(d).copy();
                    newDimension.setInsertionPosition(crt);
                    overLayout.add(newDimension);
                } else
                {
                    _errors.add(String.format("Dimension %s is not defined", d));
                }
            }
        }
        return overLayout;
    }

    /**
     * Get the correct layout for a specific field if one is defined. Default layout
     * otherwise.
     *
     * @param dataId Data identifier.
     * @return Layout for the given field id.
     */
    public List<DimensionDefinition> getLayoutForData(Configuration cfg, String dataId)
    {
        if (_scaModelConfig)
        {
            if (getLocalModelConfig().hasLayout(dataId))
            {
                return getLocalModelConfig().getLayout(dataId);
            }
            return cfg.getModelConfig().getDefaultLayout();
        } else
        {
            if (_localModelConfig.hasLayout(dataId))
            {
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
    public List<DimensionDefinition> getDefaultLayoutReversed(Configuration cfg)
    {
        List<DimensionDefinition> tmp = new ArrayList<>(getDefaultLayout(cfg));
        Collections.reverse(tmp);
        return tmp;
    }

    /**
     * Get the default layout dimensions from the local or global configuration.
     *
     * @return List of dimensions from the default layout.
     */
    public List<DimensionDefinition> getDefaultLayout(Configuration cfg)
    {
        if (_scaModelConfig)
        {
            return cfg.getModelConfig().getDefaultLayout();
        } else
        {
            return _localModelConfig.getDefaultLayout();
        }
    }

    /**
     * Get the data values extracted from the data over clause.
     *
     * @return Array identifier.
     */
    public Set<String> getDataOverClauseValues()
    {
        return _overDataValues;
    }

    /**
     * Get the reshape extracted information.
     *
     * @return List of ReshapeInfo objects containing the extracted information from
     *         the reshape clause.
     */
    public List<ReshapeInfo> getReshapeClauseValues()
    {
        return _reshapeInfos;
    }

    /**
     * Set the reshape clause extracted information.
     *
     * @param infos List of ReshapeInfo objects containing the extracted information
     *              from the reshape clause.
     */
    public void setReshapeClauseValues(List<ReshapeInfo> infos)
    {
        setClause(ClawClause.RESHAPE);
        _reshapeInfos = infos;
    }

    /**
     * Add a new dimension extracted from the directive.
     *
     * @param dimension DimensionDefinition object constructed from the value
     *                  extracted in the clause.
     */
    public void addDimension(DimensionDefinition dimension)
    {
        setClause(ClawClause.DIMENSION);
        _localModelConfig.putDimension(dimension);
    }

    /**
     * Get the copy clause value.
     *
     * @return Copy clause value as a DataMovement enum value.
     */
    public DataMovement getCopyClauseValue()
    {
        return _copyClauseValue;
    }

    /**
     * Set the copy clause value and the copy clause usage flag to true.
     *
     * @param value New copy clause value.
     */
    public void setCopyClauseValue(DataMovement value)
    {
        setClause(ClawClause.COPY);
        _copyClauseValue = value;
    }

    /**
     * Get the update clause value.
     *
     * @return Update clause value as a DataMovement enum value.
     */
    public DataMovement getUpdateClauseValue()
    {
        return _updateClauseValue;
    }

    /**
     * Set the update clause value and the update clause usage flag to true.
     *
     * @param value New update clause value.
     */
    public void setUpdateClauseValue(DataMovement value)
    {
        setClause(ClawClause.UPDATE);
        _updateClauseValue = value;
    }

    /**
     * Get the target clause value.
     *
     * @return Target clause value as a list of Target enum value.
     */
    public List<Target> getTargetClauseValues()
    {
        return _targetClauseValues;
    }

    /**
     * Set the target clause value and the update clause usage flag to true.
     *
     * @param values New target clause values.
     */
    public void setTargetClauseValue(List<Target> values)
    {
        setClause(ClawClause.TARGET);
        _targetClauseValues = values;
    }

    /**
     * Get the constraint clause value.
     *
     * @return Constraint clause value as an enum value.
     */
    public ClawConstraint getConstraintClauseValue()
    {
        return _constraintClauseValue;
    }

    /**
     * Set the constraint clause value and the update clause usage flag to true.
     *
     * @param value New constraint clause value.
     */
    public void setConstraintClauseValue(ClawConstraint value)
    {
        setClause(ClawClause.CONSTRAINT);
        _constraintClauseValue = value;
    }

    /**
     * Attach the pragma related to this CLAW language analysis.
     *
     * @param pragma Raw pragma element object.
     */
    private void attachPragma(Xnode pragma)
    {
        _pragma = pragma;
    }

    /**
     * Check if the current transformation target (defined by user) match with the
     * target defined in the clause.
     *
     * @return True if the targets matches.
     */
    public boolean isApplicableToCurrentTarget(Context context)
    {
        return _targetClauseValues == null || _targetClauseValues.isEmpty()
                || _targetClauseValues.contains(context.getTarget());
    }

    /**
     * Get the cleanup clause value.
     *
     * @return Cleanup clause value. NONE means both OpenACC and OpenMP.
     */
    public CompilerDirective getCleanupClauseValue()
    {
        return _cleanupClauseValue;
    }

    /**
     * Set the cleanup clause value and the update clause usage flag to true.
     *
     * @param value New compiler directive clause value.
     */
    public void setCleanupClauseValue(CompilerDirective value)
    {
        setClause(ClawClause.CLEANUP);
        _cleanupClauseValue = value;
    }

    /**
     * Set the current directive as a SCA from model config.
     */
    public void setScaModelConfig()
    {
        _scaModelConfig = true;
    }

    /**
     * Check whether the current directive is a SCA using model config.
     *
     * @return True if the current directive is a SCA using model config.
     */
    public boolean isScaModelConfig()
    {
        return _scaModelConfig;
    }

    /**
     * Return local configuration object. Used when dimensions and layouts are
     * defined directly in the pragma itself.
     *
     * @return Instance of ModelConfig object local to this pragma.
     */
    public ModelConfig getLocalModelConfig()
    {
        return _localModelConfig;
    }

    public Map<String, String> getMetadataMap()
    {
        return _metadataMap;
    }

    public void addMetadata(String key, String value)
    {
        _metadataMap.put(key, value);
    }

    /**
     * Check whether any errors has been reported.
     *
     * @return True if any error reported. False otherwise.
     */
    public boolean hasErrors()
    {
        return !_errors.isEmpty();
    }

    /**
     * Get list of reported errors.
     *
     * @return List of errors.
     */
    public List<String> getErrors()
    {
        return _errors;
    }

    @Override
    public String toString()
    {
        if (_pragma != null)
        {
            return String.format("%d - %s", _pragma.lineNo(), _pragma.value());
        }
        return super.toString();
    }
}
