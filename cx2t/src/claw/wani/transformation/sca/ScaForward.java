/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 * @author clementval
 */
package claw.wani.transformation.sca;

import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import claw.shenron.transformation.Transformation;
import claw.shenron.translator.Translator;
import claw.tatsu.common.Context;
import claw.tatsu.common.Message;
import claw.tatsu.common.Target;
import claw.tatsu.common.Utility;
import claw.tatsu.directive.common.DataMovement;
import claw.tatsu.directive.common.Directive;
import claw.tatsu.primitive.Field;
import claw.tatsu.primitive.Function;
import claw.tatsu.primitive.Loop;
import claw.tatsu.primitive.Type;
import claw.tatsu.primitive.Xmod;
import claw.tatsu.xcodeml.abstraction.DimensionDefinition;
import claw.tatsu.xcodeml.abstraction.FunctionCall;
import claw.tatsu.xcodeml.abstraction.InsertionPosition;
import claw.tatsu.xcodeml.abstraction.NestedDoStatement;
import claw.tatsu.xcodeml.abstraction.PromotionInfo;
import claw.tatsu.xcodeml.abstraction.Xblock;
import claw.tatsu.xcodeml.exception.IllegalTransformationException;
import claw.tatsu.xcodeml.xnode.XnodeUtil;
import claw.tatsu.xcodeml.xnode.common.Xattr;
import claw.tatsu.xcodeml.xnode.common.Xcode;
import claw.tatsu.xcodeml.xnode.common.XcodeProgram;
import claw.tatsu.xcodeml.xnode.common.Xid;
import claw.tatsu.xcodeml.xnode.common.Xnode;
import claw.tatsu.xcodeml.xnode.common.Xscope;
import claw.tatsu.xcodeml.xnode.common.XstorageClass;
import claw.tatsu.xcodeml.xnode.fortran.DeclarationPosition;
import claw.tatsu.xcodeml.xnode.fortran.FbasicType;
import claw.tatsu.xcodeml.xnode.fortran.FfunctionDefinition;
import claw.tatsu.xcodeml.xnode.fortran.FfunctionType;
import claw.tatsu.xcodeml.xnode.fortran.FmoduleDefinition;
import claw.tatsu.xcodeml.xnode.fortran.FortranModule;
import claw.tatsu.xcodeml.xnode.fortran.FortranType;
import claw.tatsu.xcodeml.xnode.fortran.Intent;
import claw.wani.language.ClawClause;
import claw.wani.language.ClawPragma;
import claw.wani.serialization.Serialization;
import claw.wani.serialization.SerializationStep;
import claw.wani.transformation.ClawTransformation;
import claw.wani.x2t.configuration.Configuration;
import claw.wani.x2t.translator.ClawTranslator;

/**
 * The Single Column Abstraction (SCA) forward transformation applies the
 * changes in the subroutine signatures to function call and function in which
 * the call is nested if needed.
 * <p>
 * During the transformation, a new "CLAW" XcodeML module file is generated if
 * the transformation has to be applied across several file unit. This file will
 * be located in the same directory as the original XcodeML module file and has
 * the following naming structure: module_name.claw.xmod
 *
 */
public class ScaForward extends ClawTransformation
{

    private final Set<String> _promotedVar; // Promoted array from the call
    private final Map<String, PromotionInfo> _promotions; // Info about promotion
    private final Map<String, String> _fctCallMapping; // NamedValue mapping
    private FunctionCall _fCall;
    private FfunctionType _fctType;
    private FortranModule _mod = null;
    private boolean _localFct = false;
    private boolean _flatten = false;

    private NestedDoStatement _doStatements;

    private String _calledFctName; // For topological sorting
    private String _callingFctName; // For topological sorting
    private boolean _isNestedInAssignment;

    /**
     * Constructs a new Sca transformation triggered from a specific pragma.
     *
     * @param directive The directive that triggered the define transformation.
     */
    public ScaForward(ClawPragma directive)
    {
        super(directive);
        _promotedVar = new HashSet<>();
        _promotions = new HashMap<>();
        _fctCallMapping = new HashMap<>();
    }

    @Override
    public boolean analyze(XcodeProgram xcodeml, Translator translator)
    {
        Xnode next = _claw.getPragma().nextSibling();
        if (next == null)
        {
            xcodeml.addError("Directive is not followed by a valid statement.", _claw.getPragma());
            return false;
        }
        if (Xnode.isOfCode(next, Xcode.EXPR_STATEMENT) || Xnode.isOfCode(next, Xcode.F_ASSIGN_STATEMENT))
        {
            _isNestedInAssignment = Xnode.isOfCode(next, Xcode.F_ASSIGN_STATEMENT);
            Xnode fctCallNode = next.matchSeq(Xcode.FUNCTION_CALL);
            if (fctCallNode != null)
            {
                _fCall = new FunctionCall(fctCallNode);
                return analyzeForward(xcodeml);
            }
        } else if (Xnode.isOfCode(next, Xcode.F_DO_STATEMENT))
        {
            _doStatements = new NestedDoStatement(next);
            return analyzeForwardWithDo(xcodeml);
        }
        xcodeml.addError("Directive is not followed by a valid statement.", _claw.getPragma());
        return false;
    }

    /**
     * Analyze the directive when it is used just before a do statement.
     *
     * @param xcodeml Current XcodeML file unit.
     * @return True if the analysis succeed. False otherwise.
     */
    private boolean analyzeForwardWithDo(XcodeProgram xcodeml)
    {
        _flatten = true;
        if (_doStatements == null)
        {
            xcodeml.addError("Directive is not followed by do statement.", _claw.getPragma());
            return false;
        }

        // Try to locate the fct call inside of the do statements. Can be nested.
        return analyzeNestedDoStmts(xcodeml);
    }

    /**
     * Recursively analyze nested do statements in order to matchSeq the call
     * statements.
     *
     * @param xcodeml Current XcodeML file unit.
     * @return True if the analysis succeed. False otherwise.
     */
    private boolean analyzeNestedDoStmts(XcodeProgram xcodeml)
    {
        for (int i = 0; i < _doStatements.size(); ++i)
        {
            if (i == _doStatements.size() - 1)
            {
                if (_doStatements.get(i).body() == null)
                {
                    xcodeml.addError("Cannot locate function call.", _claw.getPragma());
                    return false;
                }
            }
            for (Xnode n : _doStatements.get(i).body().children())
            {
                if (n.is(Xcode.F_DO_STATEMENT))
                {
                    continue;
                }
                if (!n.is(Xcode.F_PRAGMA_STATEMENT) && !n.is(Xcode.EXPR_STATEMENT))
                {
                    xcodeml.addError("Only pragmas, comments and function calls allowed " + "in the do statements.",
                            _claw.getPragma());
                    return false;
                } else if (n.is(Xcode.EXPR_STATEMENT) || n.is(Xcode.F_ASSIGN_STATEMENT))
                {
                    Xnode fctCallNode = n.matchSeq(Xcode.FUNCTION_CALL);
                    if (fctCallNode != null)
                    {
                        _fCall = new FunctionCall(fctCallNode);
                        return analyzeForward(xcodeml);
                    }
                }
            }
        }

        xcodeml.addError("Function call not found.", _claw.getPragma());
        return false;
    }

    /**
     * Analyze the directive when it is used just before a function call.
     *
     * @param xcodeml Current XcodeML file unit.
     * @return True if the analysis succeed. False otherwise.
     */
    private boolean analyzeForward(XcodeProgram xcodeml)
    {
        final Context context = xcodeml.context();
        if (_fCall == null)
        {
            xcodeml.addError("Directive is not followed by a fct call.", _claw.getPragma());
            return false;
        }

        detectParameterMapping(context, _fCall);

        _calledFctName = _fCall.getFctName();

        FfunctionDefinition fctDef = xcodeml.getGlobalDeclarationsTable().getFunctionDefinition(_calledFctName);
        FfunctionDefinition parentFctDef = _claw.getPragma().findParentFunction();
        if (parentFctDef == null)
        {
            xcodeml.addError("SCA directive is not nested in a " + "function/subroutine.", _claw.getPragma());
            return false;
        }

        FmoduleDefinition parentModule = parentFctDef.findParentModule();

        if (_fCall.isTbpCall())
        {
            /*
             * If type is a FbasicType element for a type-bound procedure, we have to
             * matchSeq the correct function in the typeTable. TODO if there is a rename.
             * TODO generic call
             */
            Xid id = parentModule.getSymbolTable().get(_calledFctName);
            if (id == null)
            {
                List<Xnode> uses = parentFctDef.getDeclarationTable().uses();
                uses.addAll(parentModule.getDeclarationTable().uses());
                if (!findInModule(context, uses))
                {
                    xcodeml.addError("Function definition not found in module ", _claw.getPragma());
                    return false;
                }
            } else
            {
                _fctType = xcodeml.getTypeTable().getFunctionType(id);
            }
        } else
        {
            if (xcodeml.getTypeTable().isFunctionType(_fCall))
            {
                _fctType = xcodeml.getTypeTable().getFunctionType(_fCall);
            } else
            {
                xcodeml.addError("Unsupported type of XcodeML/F element for the function " + _calledFctName,
                        _claw.getPragma());
                return false;
            }
        }

        /*
         * if(_fctType.isElemental() && Context.isTarget(Target.CPU)) { return true; }
         */

        /*
         * Workaround for a bug in OMNI Compiler. Look at test case claw/abstraction10.
         * In this test case, the XcodeML/F intermediate representation for the function
         * call points to a FfunctionType element with no parameters. Thus, we have to
         * matchSeq the correct FfunctionType for the same function/subroutine with the
         * same name in the module symbol table.
         */
        if (_fctType.getParameters().isEmpty())
        {
            /*
             * If not, try to matchSeq the correct FfunctionType in the module definitions
             */
            Xid id = (parentModule == null) ? null : parentModule.getSymbolTable().get(_calledFctName);

            if (id == null)
            {
                // Function is not located in the current module.
                List<Xnode> uses = parentFctDef.getDeclarationTable().uses();
                if (parentModule != null)
                {
                    uses.addAll(parentModule.getDeclarationTable().uses());
                }
                if (!findInModule(context, uses))
                {
                    xcodeml.addError(String.format("Function definition %s not found in module.", _calledFctName),
                            _claw.getPragma());
                    return false;
                }
            } else
            {
                _fctType = xcodeml.getTypeTable().getFunctionType(id);
                if (_fctType == null)
                {
                    xcodeml.addError("Called function cannot be found in the same module ", _claw.getPragma());
                    return false;
                }
            }
        }
        // end of workaround

        _callingFctName = parentFctDef.getName();
        if (_fctType != null && fctDef != null)
        {
            _localFct = true;
        } else
        {

            // Has been found already
            if (_fctType != null && _calledFctName == null)
            {
                return true;
            }

            // Get all the use statements in the fct and module definitions
            List<Xnode> uses = parentFctDef.getDeclarationTable().uses();
            if (parentModule != null)
            {
                uses.addAll(parentModule.getDeclarationTable().uses());
            }

            // Try to locate the fct in the modules defined in use statements
            if (findInModule(context, uses))
            {
                return true;
            }

            xcodeml.addError("Function signature not found in the current module.", _claw.getPragma());
            return false;
        }

        return true;
    }

    /**
     * Get all the mapping between local variable and parameter names in the
     * function call.
     *
     * @param fctCall Function call to be analyzed.
     */
    private void detectParameterMapping(Context context, FunctionCall fctCall)
    {
        if (!Xnode.isOfCode(fctCall, Xcode.FUNCTION_CALL))
        {
            return;
        }
        for (Xnode arg : fctCall.arguments())
        {
            if (arg.is(Xcode.NAMED_VALUE))
            {
                String originalName = arg.getAttribute(Xattr.NAME);
                Xnode targetVar = arg.matchDescendant(Xcode.VAR);
                if (targetVar != null)
                {
                    _fctCallMapping.put(originalName, targetVar.value());

                    Message.debug(context, "Fct parameter mapping: original_name=" + originalName + " target_name="
                            + targetVar.value());
                }
            }
        }
    }

    /**
     * Find a function in modules.
     *
     * @param useDecls List of all USE statement declarations available for search.
     * @return True if the function was found. False otherwise.
     */
    private boolean findInModule(Context context, List<Xnode> useDecls)
    {
        // TODO handle rename
        for (Xnode d : useDecls)
        {
            // Check whether a CLAW module file is available.
            _mod = Xmod.findClaw(context, d.getAttribute(Xattr.NAME));
            if (_mod != null)
            {
                Message.debug(context, "Reading CLAW module file: " + _mod.getFullPath(context));
                if (_mod.getIdentifiers().contains(_calledFctName))
                {
                    _fctType = _mod.findFunctionTypeFromCall(_fCall);
                    if (_fctType != null)
                    {
                        return true;
                    }
                }
            }
        }
        return false;
    }

    @Override
    public void transform(XcodeProgram xcodeml, Translator translator, Transformation other) throws Exception
    {
        if (_flatten)
        {
            transformFlatten(xcodeml, translator);
        } else
        {
            transformStd(xcodeml, translator);
        }

        removePragma();
    }

    /**
     * Do the flatten transformation for the forward directive. This transformation
     * adapt the function call nested in the do statements and removes those do
     * statements. The containing subroutine is not adapted.
     *
     * @param xcodeml    Current XcodeML file unit.
     * @param translator Current translator.
     * @throws Exception If something goes wrong.
     */
    private void transformFlatten(XcodeProgram xcodeml, Translator translator) throws Exception
    {
        Loop.extractBody(_doStatements);
        _doStatements.getOuterStatement().delete();
        transformStd(xcodeml, translator);
    }

    /**
     * Do the standard transformation for the forward directive. This transformation
     * adapt the function call and replicates any necessary changes to the
     * containing subroutine.
     *
     * @param xcodeml    Current XcodeML file unit.
     * @param translator Current translator.
     * @throws Exception If something goes wrong.
     */
    private void transformStd(XcodeProgram xcodeml, Translator translator) throws Exception
    {
        ClawTranslator trans = (ClawTranslator) translator;

        final Context context = trans.context();
        final Configuration cfg = trans.cfg();
        FfunctionDefinition fDef = _claw.getPragma().findParentFunction();
        if (fDef == null)
        {
            throw new IllegalTransformationException("SCA directive is not " + "nested in a function/subroutine.",
                    _claw.getPragma().lineNo());
        }

        FfunctionType parentFctType = xcodeml.getTypeTable().getFunctionType(fDef);

        if (_fctType.isElemental() || _fctType.getBooleanAttribute(Xattr.WAS_ELEMENTAL))
        {
            _flatten = true;
        }

        List<Xnode> params = _fctType.getParameters();

        /*
         * Compute the position of the first new arguments. In the case of a type-bound
         * procedure call, the first parameter declared in the procedure is not actually
         * passed as an argument. In this case, we add an offset of one to the starting
         * arguments. TODO the check might be change to fit with the XcodeML/F2008
         * specs. The TODO cont: attribute data_ref will probably be gone and replaced
         * by a TODO cont: FmemberRef element
         */
        int argOffset = 0;
        if (!params.isEmpty() && FortranType.STRUCT.isOfType(params.get(0).getType()) && _fCall.isTbpCall())
        {
            argOffset = 1;
        }

        // 1. Adapt function call with potential new arguments
        for (int i = 0; i < params.size(); i++)
        {
            Xnode p = params.get(i);
            String varId = p.value();
            String type;

            FbasicType paramType = xcodeml.getTypeTable().getBasicType(p);

            if (!p.getBooleanAttribute(Xattr.IS_INSERTED))
            {
                continue;
            }

            if (!fDef.getSymbolTable().contains(varId))
            {
                if (_flatten && !paramType.getBooleanAttribute(Xattr.IS_OPTIONAL))
                {
                    throw new IllegalTransformationException("Variable " + varId
                            + " must be locally defined where the last call to one_column " + "is made.",
                            _claw.getPragma().lineNo());
                }
                // Size variable have to be declared
                FbasicType bt = xcodeml.createBasicType(FortranType.INTEGER, Intent.IN);
                xcodeml.getTypeTable().add(bt);
                xcodeml.createIdAndDecl(varId, bt.getType(), XstorageClass.F_PARAM, fDef, DeclarationPosition.FIRST);
                type = bt.getType();
                Xnode param = xcodeml.createAndAddParam(varId, type, parentFctType);
                param.setBooleanAttribute(Xattr.IS_INSERTED, true);
            } else
            {

                // Var exists already. Add to the parameters if not here.
                type = fDef.getSymbolTable().get(varId).getType();

                /*
                 * If flatten mode, we do not add extra parameters to the function definition.
                 */
                if (!_flatten && !fDef.getSymbolTable().contains(varId))
                {
                    Xnode param = xcodeml.createAndAddParamIfNotExists(varId, type, parentFctType);
                    if (param != null)
                    {
                        param.setBooleanAttribute(Xattr.IS_INSERTED, true);
                    }
                }
            }

            // Add variable in the function call before the optional parameters
            Xnode arg = xcodeml.createNamedValue(varId);
            arg.append(xcodeml.createVar(type, varId, Xscope.LOCAL));
            Xnode hook = _fCall.arguments().get((i - 1) - argOffset);
            if (hook != null)
            {
                hook.insertAfter(arg);
            } else
            {
                _fCall.addArguments(arg);
            }
        }

        // In flatten mode, arguments are demoted if needed.
        if (_flatten)
        {
            for (Xnode arg : _fCall.arguments())
            {
                if (arg.is(Xcode.F_ARRAY_REF)
                        && arg.matchDirectDescendant(Arrays.asList(Xcode.INDEX_RANGE, Xcode.ARRAY_INDEX)) != null)
                {
                    List<Xnode> arrayIndexes = arg.matchAll(Xcode.ARRAY_INDEX);
                    for (Xnode n : arrayIndexes)
                    {
                        if (_doStatements != null
                                && XnodeUtil.isInductionIndex(n, _doStatements.getInductionVariables()))
                        {
                            n.insertAfter(xcodeml.createEmptyAssumedShaped());
                            XnodeUtil.safeDelete(n);
                        }
                    }
                }
            }
        } else
        {
            // 2. Adapt function/subroutine in which the function call is nested
            for (Xnode pBase : _fctType.getParameters())
            {
                String originalParam = pBase.value();
                if (_fctCallMapping.containsKey(originalParam))
                {
                    originalParam = _fctCallMapping.get(originalParam);
                }

                Xnode pUpdate = null;
                for (Xnode param : parentFctType.getParameters())
                {
                    if (originalParam.equals(param.value()))
                    {
                        pUpdate = param;
                    }
                }

                if (pUpdate == null)
                { // field is not a parameter but maybe out field
                    Xnode d = fDef.getDeclarationTable().get(originalParam);
                    if (d != null)
                    {
                        pUpdate = d.matchSeq(Xcode.NAME);
                    }
                }

                if (pUpdate != null)
                {

                    if (pUpdate.getType() == null || FortranType.isBuiltInType(pUpdate.getType()))
                    {
                        continue;
                    }

                    FbasicType typeBase = (_localFct) ? xcodeml.getTypeTable().getBasicType(pBase)
                            : _mod.getTypeTable().getBasicType(pBase);

                    FbasicType typeToUpdate = xcodeml.getTypeTable().getBasicType(pUpdate);

                    int targetDim = typeBase.getDimensions();
                    int baseDim = typeToUpdate.getDimensions();

                    // Types have different dimensions
                    if (typeBase.getDimensions() > typeToUpdate.getDimensions())
                    {
                        PromotionInfo promotionInfo = new PromotionInfo(originalParam);
                        promotionInfo.readDimensionsFromString(pBase.getAttribute(Xattr.PROMOTION_INFO));

                        FbasicType type = _localFct
                                ? Type.duplicateWithDimension(typeBase, typeToUpdate, xcodeml, xcodeml,
                                        promotionInfo.getDimensions())
                                : Type.duplicateWithDimension(typeBase, typeToUpdate, _mod, xcodeml,
                                        promotionInfo.getDimensions());

                        pUpdate.setType(type);

                        Xid id = fDef.getSymbolTable().get(originalParam);
                        if (id != null)
                        {
                            id.setType(type);
                        }
                        Xnode varDecl = fDef.getDeclarationTable().get(originalParam);
                        if (varDecl != null)
                        {
                            varDecl.matchSeq(Xcode.NAME).setType(type);
                        }

                        promotionInfo.setBaseDimension(baseDim);
                        promotionInfo.setTargetDimension(targetDim);
                        promotionInfo.setTargetType(type);
                        _promotions.put(originalParam, promotionInfo);
                        _promotedVar.add(originalParam);

                        pBase.copyAttribute(pUpdate, Xattr.PROMOTION_INFO);
                    }
                }
            }

            if (!parentFctType.getBooleanAttribute(Xattr.IS_PRIVATE))
            {
                // 3. Replicate the change in a potential module file
                FmoduleDefinition modDef = fDef.findParentModule();
                if (modDef != null)
                {
                    Xmod.updateSignature(modDef.getName(), xcodeml, fDef, parentFctType, false);
                }
            } else if (_fCall.matchSeq(Xcode.NAME).hasAttribute(Xattr.DATA_REF))
            {
                /*
                 * The function/subroutine is private but accessible through the type as a
                 * type-bound procedure. In this case, the function is not in the type table of
                 * the .xmod file. We need to insert it first and then we can update it.
                 */
                FmoduleDefinition modDef = fDef.findParentModule();
                if (modDef != null)
                {
                    Xmod.updateSignature(modDef.getName(), xcodeml, fDef, parentFctType, true);
                }
            }
        }

        updateResultVar(xcodeml);

        propagatePromotion(xcodeml, (ClawTranslator) translator);

        Xnode fctCallAncestor = _fCall.matchAncestor(Xcode.EXPR_STATEMENT);
        if (fctCallAncestor == null)
        {
            fctCallAncestor = _fCall.matchAncestor(Xcode.F_ASSIGN_STATEMENT);
        }

        if (_claw.hasClause(ClawClause.CREATE) && context.isTarget(Target.GPU))
        {
            List<String> creates = _fCall.gatherArguments(xcodeml, _fctType, _mod, Intent.INOUT, true, false);

            if (_fctType.isFunction())
            {
                String returnValue = XnodeUtil.gatherReturnValue(xcodeml, _fCall);
                if (returnValue != null)
                {
                    creates.add(returnValue);
                }
            }

            Xblock fctCallBlock = new Xblock(fctCallAncestor);
            Directive.generateDataRegionClause(xcodeml, Collections.emptyList(), creates, fctCallBlock);
        }

        // Serialization input
        if (_claw.hasClause(ClawClause.SAVEPOINT))
        {
            List<String> inFields = _fCall.gatherArguments(xcodeml, _fctType, _mod != null ? _mod : xcodeml, Intent.IN,
                    true, false);
            Serialization.insertImports(cfg, xcodeml, _fCall.findParentFunction());
            if (context.isTarget(Target.CPU))
            {
                Serialization.generateWriteSavepoint(cfg, xcodeml, fctCallAncestor, _claw.getMetadataMap(), inFields,
                        _claw.value(ClawClause.SAVEPOINT), SerializationStep.SER_IN);
            } else
            {
                Serialization.generateReadSavepoint(cfg, xcodeml, fctCallAncestor, _claw.getMetadataMap(), inFields,
                        _claw.value(ClawClause.SAVEPOINT), SerializationStep.SER_IN);
            }
        }

        Xnode postHook = fctCallAncestor;

        if (_claw.hasClause(ClawClause.UPDATE) && context.isTarget(Target.GPU)
                && cfg.getBooleanParameter(Configuration.SCA_FORWARD_UPDATE_ENABLED))
        {
            // Generate update from HOST TO DEVICE
            if ((_claw.getUpdateClauseValue() == DataMovement.TWO_WAY
                    || _claw.getUpdateClauseValue() == DataMovement.HOST_TO_DEVICE) && cfg.updateAtInput())
            {
                List<String> in = _fCall.gatherArguments(xcodeml, _fctType, _mod != null ? _mod : xcodeml, Intent.IN,
                        true, false);

                Directive.generateUpdate(xcodeml, fctCallAncestor, in, DataMovement.HOST_TO_DEVICE);
            }

            // Generate update from DEVICE to HOST
            if ((_claw.getUpdateClauseValue() == DataMovement.TWO_WAY
                    || _claw.getUpdateClauseValue() == DataMovement.DEVICE_TO_HOST) && cfg.updateAtOutput())
            {
                List<String> out = _fCall.gatherArguments(xcodeml, _fctType, _mod != null ? _mod : xcodeml, Intent.OUT,
                        true, false);

                if (_fctType.isFunction())
                {
                    String returnValue = XnodeUtil.gatherReturnValue(xcodeml, _fCall);
                    if (returnValue != null)
                    {
                        out.add(returnValue);
                    }
                }

                postHook = Directive.generateUpdate(xcodeml, fctCallAncestor, out, DataMovement.DEVICE_TO_HOST);
            }
        }

        if (_claw.hasClause(ClawClause.PARALLEL) && context.isTarget(Target.GPU))
        {
            Directive.generateParallelRegion(xcodeml, fctCallAncestor, fctCallAncestor);
        }

        // Serialization output
        if (_claw.hasClause(ClawClause.SAVEPOINT))
        {
            List<String> outFields = _fCall.gatherArguments(xcodeml, _fctType, _mod != null ? _mod : xcodeml,
                    Intent.OUT, true, false);
            Serialization.insertImports(cfg, xcodeml, _fCall.findParentFunction());
            Serialization.generateWriteSavepoint(cfg, xcodeml, postHook, _claw.getMetadataMap(), outFields,
                    _claw.value(ClawClause.SAVEPOINT), SerializationStep.SER_OUT);
        }
    }

    /**
     * Apply promotion to the result return variable of a forward call.
     *
     * @param xcodeml Current XcodeML program unit.
     * @throws IllegalTransformationException If XcodeML transformation cannot be
     *                                        done.
     */
    private void updateResultVar(XcodeProgram xcodeml) throws IllegalTransformationException
    {
        if (_isNestedInAssignment)
        {
            Xnode assignment = _claw.getPragma().nextSibling();
            if (assignment == null || !_fctType.hasAttribute(Xattr.PROMOTION_INFO))
            {
                return;
            }

            Xnode lhs = assignment.firstChild();
            // TODO handle the case when the array ref is a var directly
            Xnode varInLhs = lhs.matchDescendant(Xcode.VAR);
            FfunctionDefinition parentFctDef = _fCall.findParentFunction();

            PromotionInfo promotionInfo;
            if (!_promotions.containsKey(varInLhs.value()))
            {
                // Perform the promotion on the variable

                promotionInfo = new PromotionInfo(varInLhs.value());
                promotionInfo.readDimensionsFromString(_fctType.getAttribute(Xattr.PROMOTION_INFO));
                Field.promote(promotionInfo, parentFctDef, xcodeml);
                _promotions.put(varInLhs.value(), promotionInfo);
                _promotedVar.add(varInLhs.value());
            } else
            {
                promotionInfo = _promotions.get(varInLhs.value());
            }

            // Adapt array index to reflect the new return type
            if (Xnode.isOfCode(lhs, Xcode.F_ARRAY_REF))
            {
                for (int i = 0; i < promotionInfo.diffDimension(); ++i)
                {
                    Xnode indexRange = xcodeml.createEmptyAssumedShaped();
                    lhs.append(indexRange);
                }
            } else
            {
                throw new IllegalTransformationException("Unsupported return " + "variable for promotion.",
                        _claw.getPragma().lineNo());
            }

            // If the array is a target, check if we have to promote a pointer
            Field.adaptPointer(xcodeml, parentFctDef, _promotions, promotionInfo);
        }
    }

    /**
     * Propagate possible promotion in assignments statements in the parent
     * subroutine of the function call.
     *
     * @param xcodeml    Current XcodeML program unit.
     * @param translator Current translator to store information between
     *                   transformation.
     */
    private void propagatePromotion(XcodeProgram xcodeml, ClawTranslator translator)
            throws IllegalTransformationException
    {
        // Get all the assignment statements in the function definition
        FfunctionDefinition parentFctDef = _fCall.findParentFunction();

        // Retrieve information of previous forward transformation in the same fct
        List<String> previouslyPromoted = Utility.convertToList(translator.hasElement(parentFctDef));

        List<Xnode> assignments = parentFctDef.matchAll(Xcode.F_ASSIGN_STATEMENT);

        // Find promotion info that can be used.
        // TODO define how default promotion is encoded in xmod file. For the
        // TODO moment using the first information found in fctType.
        PromotionInfo defaultInfo = Function.readPromotionInfo(_fctType, InsertionPosition.BEFORE);

        for (Xnode assignment : assignments)
        {
            Xnode lhs = assignment.child(0);
            Xnode rhs = assignment.child(1);

            List<Xnode> varsInRhs = rhs.matchAll(Xcode.VAR);
            for (Xnode var : varsInRhs)
            {
                // Check if the assignment statement uses a promoted variable
                if (_promotedVar.contains(var.value()) && var.matchAncestor(Xcode.FUNCTION_CALL) == null
                        && Xnode.isOfCode(lhs, Xcode.F_ARRAY_REF))
                {
                    Xnode varInLhs = lhs.matchDescendant(Xcode.VAR);
                    if (varInLhs == null)
                    {
                        throw new IllegalTransformationException("Unable to propagate " + "promotion. Internal error.",
                                _claw.getPragma().lineNo());
                    }

                    // Declare the induction variable if they are not present
                    for (DimensionDefinition dim : defaultInfo.getDimensions())
                    {
                        if (parentFctDef.getDeclarationTable().get(dim.getIdentifier()) == null)
                        {
                            xcodeml.createIdAndDecl(dim.getIdentifier(), FortranType.INTEGER, XstorageClass.F_LOCAL,
                                    parentFctDef, DeclarationPosition.LAST);
                        }
                    }

                    // Generate the do statements and move the assignment statement in
                    NestedDoStatement doStmt = new NestedDoStatement(defaultInfo.getDimensions(), xcodeml);
                    assignment.insertAfter(doStmt.getOuterStatement());
                    doStmt.getInnerStatement().body().append(assignment);

                    PromotionInfo promotionInfo;
                    if (!previouslyPromoted.contains(varInLhs.value()))
                    {
                        // Perform the promotion on the variable
                        promotionInfo = new PromotionInfo(varInLhs.value(), defaultInfo.getDimensions());
                        Field.promote(promotionInfo, parentFctDef, xcodeml);
                        _promotions.put(promotionInfo.getIdentifier(), promotionInfo);
                    } else
                    {
                        promotionInfo = _promotions.get(varInLhs.value());
                    }
                    _promotedVar.add(varInLhs.value());

                    // Adapt the reference in the assignment statement
                    for (String id : _promotedVar)
                    {
                        _promotions.get(id).resetFlags();
                        Field.adaptArrayRef(_promotions.get(id), assignment, false, xcodeml);
                    }

                    // If the array is a target, check if we have to promote a pointer
                    if (!previouslyPromoted.contains(varInLhs.value()))
                    {
                        Field.adaptPointer(xcodeml, parentFctDef, _promotions, promotionInfo);
                        previouslyPromoted.add(varInLhs.value());
                    }

                    break;
                    /*
                     * if one var in the rhs of the assignment statement was promoted it's enough
                     * and we can switch to the next assignment statement.
                     */
                }
            }
        }

        translator.storeElement(parentFctDef, previouslyPromoted);
    }

    /**
     * @return Always false as independent transformation are applied one by one.
     * @see Transformation#canBeTransformedWith(XcodeProgram, Transformation)
     */
    @Override
    public boolean canBeTransformedWith(XcodeProgram xcodeml, Transformation other)
    {
        return false; // independent transformation
    }

    /**
     * Get the called fct name.
     *
     * @return Fct name.
     */
    public String getCalledFctName()
    {
        return _calledFctName;
    }

    /**
     * Get the parent fct name.
     *
     * @return Fct name.
     */
    public String getCallingFctName()
    {
        return _callingFctName;
    }
}
