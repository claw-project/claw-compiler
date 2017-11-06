/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */
package cx2x.translator.transformation.claw.parallelize;

import cx2x.translator.ClawTranslator;
import cx2x.translator.common.ClawConstant;
import cx2x.translator.common.Utility;
import cx2x.translator.language.accelerator.AcceleratorHelper;
import cx2x.translator.language.base.ClawDMD;
import cx2x.translator.language.base.ClawLanguage;
import cx2x.translator.language.common.OverPosition;
import cx2x.translator.language.helper.TransformationHelper;
import cx2x.translator.transformation.ClawTransformation;
import cx2x.translator.transformation.helper.FieldTransform;
import cx2x.translator.xnode.ClawAttr;
import cx2x.xcodeml.exception.IllegalTransformationException;
import cx2x.xcodeml.helper.NestedDoStatement;
import cx2x.xcodeml.helper.XnodeUtil;
import cx2x.xcodeml.language.DimensionDefinition;
import cx2x.xcodeml.transformation.Transformation;
import cx2x.xcodeml.transformation.Translator;
import cx2x.xcodeml.xnode.*;
import xcodeml.util.XmOption;

import java.util.*;

/**
 * The parallelize forward transformation applies the changes in the subroutine
 * signatures to function call and function in which the call is nested if
 * needed.
 * <p>
 * During the transformation, a new "CLAW" XcodeML module file is generated
 * if the transformation has to be applied across several file unit. This
 * file will be located in the same directory as the original XcodeML module
 * file and has the following naming structure: module_name.claw.xmod
 *
 * @author clementval
 */
public class ParallelizeForward extends ClawTransformation {

  private final List<String> _promotedVar; // Promoted array from the call
  private final List<String> _promotedWithBeforeOver;
  private final List<String> _promotedWithAfterOver;
  private final List<String> _promotedWithMiddleOver;
  private final Map<String, PromotionInfo> _promotions; // Info about promotion
  private final Map<String, String> _fctCallMapping; // NamedValue mapping
  private Xnode _fctCall;
  private XfunctionType _fctType;
  private XfunctionType _parentFctType;
  private Xmod _mod = null;
  private boolean _localFct = false;
  private boolean _flatten = false;

  private NestedDoStatement _doStatements;

  private String _calledFctName;  // For topological sorting
  private String _callingFctName; // For topological sorting
  private boolean _isNestedInAssignment;

  /**
   * Constructs a new Parallelize transformation triggered from a specific
   * pragma.
   *
   * @param directive The directive that triggered the define transformation.
   */
  public ParallelizeForward(ClawLanguage directive) {
    super(directive);
    _promotedVar = new ArrayList<>();
    _promotedWithBeforeOver = new ArrayList<>();
    _promotedWithAfterOver = new ArrayList<>();
    _promotedWithMiddleOver = new ArrayList<>();
    _promotions = new HashMap<>();
    _fctCallMapping = new HashMap<>();
  }

  @Override
  public boolean analyze(XcodeProgram xcodeml, Translator translator) {
    Xnode next = _claw.getPragma().nextSibling();
    if(next == null) {
      xcodeml.addError("Directive is not followed by a valid statement.",
          _claw.getPragma().lineNo());
      return false;
    }
    if(next.opcode() == Xcode.EXPRSTATEMENT
        || next.opcode() == Xcode.FASSIGNSTATEMENT)
    {
      _isNestedInAssignment = next.opcode() == Xcode.FASSIGNSTATEMENT;
      _fctCall = next.matchSeq(Xcode.FUNCTIONCALL);
      if(_fctCall != null) {
        return analyzeForward(xcodeml);
      }
    } else if(next.opcode() == Xcode.FDOSTATEMENT) {
      _doStatements = new NestedDoStatement(next);
      return analyzeForwardWithDo(xcodeml);
    }
    xcodeml.addError("Directive is not followed by a valid statement.",
        _claw.getPragma().lineNo());
    return false;
  }

  /**
   * Analyze the directive when it is used just before a do statement.
   *
   * @param xcodeml Current XcodeML file unit.
   * @return True if the analysis succeed. False otherwise.
   */
  private boolean analyzeForwardWithDo(XcodeProgram xcodeml) {
    _flatten = true;
    if(_doStatements == null) {
      xcodeml.addError("Directive is not followed by do statement.",
          _claw.getPragma().lineNo());
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
  private boolean analyzeNestedDoStmts(XcodeProgram xcodeml) {
    for(int i = 0; i < _doStatements.size(); ++i) {
      if(i == _doStatements.size() - 1) {
        if(_doStatements.get(i).body() == null) {
          xcodeml.addError("Cannot locate function call.",
              _claw.getPragma().lineNo());
          return false;
        }
      }
      for(Xnode n : _doStatements.get(i).body().children()) {
        if(n.opcode() == Xcode.FDOSTATEMENT) {
          continue;
        }
        if(n.opcode() != Xcode.FPRAGMASTATEMENT
            && n.opcode() != Xcode.EXPRSTATEMENT)
        {
          xcodeml.addError("Only pragmas, comments and function calls allowed "
              + "in the do statements.", _claw.getPragma().lineNo());
          return false;
        } else if(n.opcode() == Xcode.EXPRSTATEMENT
            || n.opcode() == Xcode.FASSIGNSTATEMENT)
        {
          _fctCall = n.matchSeq(Xcode.FUNCTIONCALL);
          if(_fctCall != null) {
            return analyzeForward(xcodeml);
          }
        }
      }
    }

    xcodeml.addError("Function call not found.", _claw.getPragma().lineNo());
    return false;
  }

  /**
   * Analyze the directive when it is used just before a function call.
   *
   * @param xcodeml Current XcodeML file unit.
   * @return True if the analysis succeed. False otherwise.
   */
  private boolean analyzeForward(XcodeProgram xcodeml) {
    if(_fctCall == null) {
      xcodeml.addError("Directive is not followed by a fct call.",
          _claw.getPragma().lineNo());
      return false;
    }

    detectParameterMapping(_fctCall);

    boolean isTypeBoundProcedure = false;
    if(_fctCall.firstChild().opcode() == Xcode.FMEMBERREF) {
      isTypeBoundProcedure = true;
      _calledFctName = _fctCall.firstChild().getAttribute(Xattr.MEMBER);
    } else {
      _calledFctName = _fctCall.matchSeq(Xcode.NAME).value();
    }

    XfunctionDefinition fctDef = xcodeml.getGlobalDeclarationsTable().
        getFunctionDefinition(_calledFctName);
    XfunctionDefinition parentFctDef = _claw.getPragma().findParentFunction();
    if(parentFctDef == null) {
      xcodeml.addError("Parallelize directive is not nested in a " +
          "function/subroutine.", _claw.getPragma().lineNo());
      return false;
    }

    XmoduleDefinition parentModule = parentFctDef.findParentModule();

    if(isTypeBoundProcedure) {
      /* If type is a FbasicType element for a type-bound procedure, we have to
       * matchSeq the correct function in the typeTable.
       * TODO if there is a rename.
       * TODO generic call */
      Xid id = parentModule.getSymbolTable().get(_calledFctName);
      if(id == null) {
        List<Xnode> uses = parentFctDef.getDeclarationTable().getAllUseStmts();
        uses.addAll(parentModule.getDeclarationTable().getAllUseStmts());
        if(!findInModule(uses)) {
          xcodeml.addError("Function definition not found in module ",
              _claw.getPragma().lineNo());
          return false;
        }
      } else {
        _fctType = xcodeml.getTypeTable().getFunctionType(id);
      }
    } else {
      if(xcodeml.getTypeTable().isFunctionType(_fctCall)) {
        _fctType = xcodeml.getTypeTable().getFunctionType(_fctCall);
      } else {
        xcodeml.addError("Unsupported type of XcodeML/F element for the function "
            + _calledFctName, _claw.getPragma().lineNo());
        return false;
      }
    }

    /* Workaround for a bug in OMNI Compiler. Look at test case
     * claw/abstraction10. In this test case, the XcodeML/F intermediate
     * representation for the function call points to a FfunctionType element
     * with no parameters. Thus, we have to matchSeq the correct FfunctionType
     * for the same function/subroutine with the same name in the module
     * symbol table. */
    if(_fctType.getParameterNb() == 0) {
      // If not, try to matchSeq the correct FfunctionType in the module definitions
      Xid id = parentModule.getSymbolTable().get(_calledFctName);

      if(id == null) {
        // Function is not located in the current module.
        List<Xnode> uses = parentFctDef.getDeclarationTable().getAllUseStmts();
        uses.addAll(parentModule.getDeclarationTable().getAllUseStmts());
        if(!findInModule(uses)) {
          xcodeml.addError("Function definition not found in module ",
              _claw.getPragma().lineNo());
          return false;
        }
      } else {
        _fctType = xcodeml.getTypeTable().getFunctionType(id);
        if(_fctType == null) {
          xcodeml.addError(
              "Called function cannot be found in the same module ",
              _claw.getPragma().lineNo());
          return false;
        }
      }
    }
    // end of workaround

    _callingFctName = parentFctDef.getName().value();
    if(_fctType != null && fctDef != null) {
      _localFct = true;
    } else {

      // Has been found already
      if(_fctType != null && _calledFctName == null) {
        return true;
      }

      // Get all the use statements in the fct and module definitions
      List<Xnode> uses = parentFctDef.getDeclarationTable().getAllUseStmts();
      if(parentModule != null) {
        uses.addAll(parentModule.getDeclarationTable().getAllUseStmts());
      }

      // Try to locate the fct in the modules defined in use statements
      if(findInModule(uses)) {
        return true;
      }

      xcodeml.addError("Function signature not found in the current module.",
          _claw.getPragma().lineNo());
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
  private void detectParameterMapping(Xnode fctCall) {
    if(fctCall == null || fctCall.opcode() != Xcode.FUNCTIONCALL) {
      return;
    }
    for(Xnode arg : _fctCall.matchSeq(Xcode.ARGUMENTS).children()) {
      if(arg.opcode() == Xcode.NAMEDVALUE) {
        String original_name = arg.getAttribute(Xattr.NAME);
        Xnode target_var = arg.matchDescendant(Xcode.VAR);
        if(target_var != null) {
          _fctCallMapping.put(original_name, target_var.value());

          if(XmOption.isDebugOutput()) {
            System.out.println("Fct parameter mapping: original_name=" +
                original_name + " target_name=" + target_var.value());
          }
        }
      }
    }
  }

  /**
   * Find a function in modules.
   *
   * @param useDecls List of all USE statement declarations available for
   *                 search.
   * @return True if the function was found. False otherwise.
   */
  private boolean findInModule(List<Xnode> useDecls) {
    // TODO handle rename
    for(Xnode d : useDecls) {

      // Check whether a CLAW file is available.
      _mod = TransformationHelper.locateClawModuleFile(_claw.getTarget(),
          _claw.getAcceleratorGenerator().getDirectiveLanguage(),
          d.getAttribute(Xattr.NAME));

      if(_mod != null) {

        // debug information
        if(XmOption.isDebugOutput()) {
          System.out.println("Reading CLAW module file: " +
              _mod.getFullPath(ClawConstant.CLAW_MOD_SUFFIX));
        }

        if(_mod.getIdentifiers().contains(_calledFctName)) {
          Xid id = _mod.getIdentifiers().get(_calledFctName);
          _fctType = _mod.getTypeTable().getFunctionType(id);
          if(_fctType != null) {
            _calledFctName = null;
            return true;
          }
        }
      }
    }
    return false;
  }

  @Override
  public void transform(XcodeProgram xcodeml, Translator translator,
                        Transformation other) throws Exception
  {
    if(_flatten) {
      transformFlatten(xcodeml, translator);
    } else {
      transformStd(xcodeml, translator);
    }

    removePragma();
  }

  /**
   * Do the flatten transformation for the forward directive. This
   * transformation adapt the function call nested in the do statements and
   * removes those do statements. The containing subroutine is not adapted.
   *
   * @param xcodeml    Current XcodeML file unit.
   * @param translator Current translator.
   * @throws Exception If something goes wrong.
   */
  private void transformFlatten(XcodeProgram xcodeml, Translator translator)
      throws Exception
  {
    XnodeUtil.extractBody(_doStatements);
    _doStatements.getOuterStatement().delete();
    transformStd(xcodeml, translator);
  }

  /**
   * Do the standard transformation for the forward directive. This
   * transformation adapt the function call and replicates any necessary changes
   * to the containing subroutine.
   *
   * @param xcodeml    Current XcodeML file unit.
   * @param translator Current translator.
   * @throws Exception If something goes wrong.
   */
  private void transformStd(XcodeProgram xcodeml, Translator translator)
      throws Exception
  {
    XfunctionDefinition fDef = _claw.getPragma().findParentFunction();
    if(fDef == null) {
      throw new IllegalTransformationException("Parallelize directive is not " +
          "nested in a function/subroutine.", _claw.getPragma().lineNo());
    }
    _parentFctType = xcodeml.getTypeTable().getFunctionType(fDef);

    List<Xnode> params = _fctType.getParams().getAll();

    /* Compute the position of the first new arguments. In the case of a
     * type-bound procedure call, the first parameter declared in the procedure
     * is not actually passed as an argument. In this case, we add an offset of
     * one to the starting arguments.
     * TODO the check might be change to fit with the XcodeML/F2008 specs. The
     * TODO cont: attribute data_ref will probably be gone and replaced by a
     * TODO cont: FmemberRef element
     */
    int argOffset = 0;
    if(XcodeType.STRUCT.isOfType(params.get(0).getType())
        && _fctCall.firstChild().opcode().equals(Xcode.FMEMBERREF))
    {
      argOffset = 1;
    }

    // 1. Adapt function call with potential new arguments
    for(int i = 0; i < params.size(); i++) {
      Xnode p = params.get(i);
      //String var = p.value();
      String varId = p.value();
      String type;

      XbasicType paramType = xcodeml.getTypeTable().getBasicType(p);

      if(!p.getBooleanAttribute(ClawAttr.IS_CLAW.toString())) {
        continue;
      }

      if(!fDef.getSymbolTable().contains(varId)) {
        if(_flatten && !paramType.getBooleanAttribute(Xattr.IS_OPTIONAL)) {
          throw new IllegalTransformationException("Variable " + varId +
              " must be locally defined where the last call to parallelize " +
              "is made.", _claw.getPragma().lineNo());
        }
        // Size variable have to be declared
        XbasicType bt = xcodeml.createBasicType(XbuiltInType.INT, Xintent.IN);
        xcodeml.getTypeTable().add(bt);
        xcodeml.createIdAndDecl(varId, bt.getType(),
            XstorageClass.F_PARAM, fDef, true);
        type = bt.getType();
        Xnode param = xcodeml.createAndAddParam(varId, type, _parentFctType);
        param.setBooleanAttribute(ClawAttr.IS_CLAW.toString(), true);
      } else {

        // Var exists already. Add to the parameters if not here.
        type = fDef.getSymbolTable().get(varId).getType();

        /* If flatten mode, we do not add extra parameters to the function
         * definition */
        if(!_flatten) {
          Xnode param =
              xcodeml.createAndAddParamIfNotExists(varId, type, _parentFctType);
          if(param != null) {
            param.setBooleanAttribute(ClawAttr.IS_CLAW.toString(), true);
          }
        }
      }

      // Add variable in the function call before the optional parameters
      Xnode arg = xcodeml.createNamedValue(varId);
      arg.append(xcodeml.createVar(type, varId, Xscope.LOCAL));
      Xnode arguments = _fctCall.matchSeq(Xcode.ARGUMENTS);
      Xnode hook = arguments.child((i - 1) - argOffset);
      hook.insertAfter(arg);
    }

    // In flatten mode, arguments are demoted if needed.
    if(_flatten) {
      Xnode arguments = _fctCall.matchSeq(Xcode.ARGUMENTS);
      for(Xnode arg : arguments.children()) {
        if(arg.opcode() == Xcode.FARRAYREF && arg.matchDirectDescendant(
            Arrays.asList(Xcode.INDEXRANGE, Xcode.ARRAYINDEX)) != null)
        {
          List<Xnode> arrayIndexes = arg.matchAll(Xcode.ARRAYINDEX);
          for(Xnode n : arrayIndexes) {
            if(XnodeUtil.isInductionIndex(n,
                _doStatements.getInductionVariables()))
            {
              n.insertAfter(xcodeml.createEmptyAssumedShaped());
              XnodeUtil.safeDelete(n);
            }
          }
        }
      }
    } else {
      // 2. Adapt function/subroutine in which the function call is nested
      for(Xnode pBase : _fctType.getParams().getAll()) {
        String original_param = pBase.value();
        if(_fctCallMapping.containsKey(original_param)) {
          original_param = _fctCallMapping.get(original_param);
        }

        Xnode pUpdate = null;
        for(Xnode param : _parentFctType.getParams().getAll()) {
          if(original_param.equals(param.value())) {
            pUpdate = param;
          }
        }

        if(pUpdate == null) { // field is not a parameter but maybe out field
          Xnode d = fDef.getDeclarationTable().get(original_param);
          if(d != null) {
            pUpdate = d.matchSeq(Xcode.NAME);
          }
          // TODO handle deferred shape
        }

        if(pUpdate != null) {

          if(pUpdate.getType() == null
              || XcodeType.isBuiltInType(pUpdate.getType()))
          {
            continue;
          }

          XbasicType typeBase = (_localFct) ?
              xcodeml.getTypeTable().getBasicType(pBase)
              : _mod.getTypeTable().getBasicType(pBase);

          XbasicType typeToUpdate = xcodeml.getTypeTable().getBasicType(pUpdate);

          int targetDim = typeBase.getDimensions();
          int baseDim = typeToUpdate.getDimensions();

          // Types have different dimensions
          if(typeBase.getDimensions() > typeToUpdate.getDimensions()) {

            // TODO check intent rules
            /*if(!isParam && typeToUpdate.getIntent() != Xintent.OUT){
              continue;
            }*/

            List<DimensionDefinition> dimensions =
                TransformationHelper.findDimensions(_fctType);
            OverPosition overPos = OverPosition.fromString(
                pBase.getAttribute(ClawAttr.OVER.toString()));

            String type = _localFct ?
                TransformationHelper.duplicateWithDimension(typeBase,
                    typeToUpdate, xcodeml, xcodeml, overPos, dimensions)
                : TransformationHelper.duplicateWithDimension(typeBase,
                typeToUpdate, xcodeml, _mod, overPos, dimensions);

            pUpdate.setType(type);

            Xid id = fDef.getSymbolTable().get(original_param);
            if(id != null) {
              id.setType(type);
            }
            Xnode varDecl = fDef.getDeclarationTable().get(original_param);
            if(varDecl != null) {
              varDecl.matchSeq(Xcode.NAME).setType(type);
            }

            _promotedVar.add(original_param);

            addPromotedVar(original_param, overPos);

            _promotions.put(original_param, new PromotionInfo(
                pBase.value(), baseDim, targetDim, type));
          }
        }
      }

      if(!_parentFctType.getBooleanAttribute(Xattr.IS_PRIVATE)) {
        // 3. Replicate the change in a potential module file
        XmoduleDefinition modDef = fDef.findParentModule();
        TransformationHelper.updateModuleSignature(xcodeml, fDef,
            _parentFctType, modDef, _claw, translator, false);
      } else if(_fctCall.matchSeq(Xcode.NAME).hasAttribute(Xattr.DATA_REF)) {
        /* The function/subroutine is private but accessible through the type
         * as a type-bound procedure. In this case, the function is not in the
         * type table of the .xmod file. We need to insert it first and then
         * we can update it. */
        XmoduleDefinition modDef = fDef.findParentModule();
        TransformationHelper.updateModuleSignature(xcodeml, fDef,
            _parentFctType, modDef, _claw, translator, true);
      }
    }

    updateResultVar(xcodeml);

    propagatePromotion(xcodeml, (ClawTranslator) translator);

    Xnode exprStmt = _fctCall.matchAncestor(Xcode.EXPRSTATEMENT);
    if(_claw.hasCreateClause()) {
      List<String> creates =
          XnodeUtil.gatherArguments(xcodeml, _fctCall, Xintent.INOUT, true);
      AcceleratorHelper.generateDataRegionClause(_claw, xcodeml,
          Collections.<String>emptyList(), creates, exprStmt, exprStmt);
    }

    if(_claw.hasUpdateClause()) {
      if(_claw.getUpdateClauseValue() == ClawDMD.BOTH ||
          _claw.getUpdateClauseValue() == ClawDMD.DEVICE)
      {
        List<String> out =
            XnodeUtil.gatherArguments(xcodeml, _fctCall, Xintent.IN, true);
        AcceleratorHelper.generateUpdate(_claw, xcodeml, exprStmt, out,
            ClawDMD.DEVICE);
      }

      if(_claw.getUpdateClauseValue() == ClawDMD.BOTH ||
          _claw.getUpdateClauseValue() == ClawDMD.HOST)
      {
        List<String> out =
            XnodeUtil.gatherArguments(xcodeml, _fctCall, Xintent.OUT, true);
        AcceleratorHelper.generateUpdate(_claw, xcodeml, exprStmt, out,
            ClawDMD.HOST);
      }
    }
  }

  /**
   * Apply promotion to the result return variable of a forward call.
   *
   * @param xcodeml Current XcodeML program unit.
   * @throws IllegalTransformationException If XcodeML transformation cannot be
   *                                        done.
   */
  private void updateResultVar(XcodeProgram xcodeml)
      throws IllegalTransformationException
  {
    if(_isNestedInAssignment) {
      Xnode assignment = _claw.getPragma().nextSibling();
      if(assignment == null
          || !_fctType.hasAttribute(ClawAttr.OVER.toString()))
      {
        return;
      }

      OverPosition overPos = OverPosition.fromString(
          _fctType.getAttribute(ClawAttr.OVER.toString()));

      Xnode lhs = assignment.child(0);
      // TODO handle the case when the array ref is a var directly
      Xnode varInLhs = lhs.matchDescendant(Xcode.VAR);

      List<DimensionDefinition> dimensions =
          TransformationHelper.findDimensions(_parentFctType);
      XfunctionDefinition parentFctDef = _fctCall.findParentFunction();

      XbasicType varType = xcodeml.getTypeTable().getBasicType(varInLhs);

      PromotionInfo promotionInfo;
      if(!_promotions.containsKey(varInLhs.value())) {
        // Perform the promotion on the variable

        promotionInfo = new PromotionInfo(varInLhs.value(), _claw);
        FieldTransform.promote(promotionInfo, parentFctDef, xcodeml);

        _promotions.put(varInLhs.value(), promotionInfo);

        addPromotedVar(varInLhs.value(), overPos);

      } else {
        promotionInfo = _promotions.get(varInLhs.value());
      }

      // Adapt array index to reflect the new return type
      if(lhs.opcode() == Xcode.FARRAYREF) {
        for(int i = 0; i < promotionInfo.diffDimension(); ++i) {
          Xnode indexRange = xcodeml.createEmptyAssumedShaped();
          lhs.append(indexRange);
        }
      /*} else if(lhs.opcode() == Xcode.VAR) {
        // TODO avoid array var without colon notation
          /* throw new IllegalTransformationException("Use the colon notation "
              + "for the return variable. This notation is not supported." +
              _claw.getPragma().value()); */
      } else {
        throw new IllegalTransformationException("Unsupported return " +
            "variable for promotion.", _claw.getPragma().lineNo());
      }

      // If the array is a target, check if we have to promote a pointer
      adaptPointer(varType, varInLhs.value(), parentFctDef, xcodeml,
          promotionInfo, dimensions);
    }
  }

  /**
   * Save promoted variable name in function of its over information.
   *
   * @param fieldId Name of the promoted variable.
   * @param overPos Over position value.
   */
  private void addPromotedVar(String fieldId, OverPosition overPos) {
    switch(overPos) {
      case BEFORE:
        _promotedWithBeforeOver.add(fieldId);
        break;
      case MIDDLE:
        _promotedWithMiddleOver.add(fieldId);
        break;
      case AFTER:
        _promotedWithAfterOver.add(fieldId);
        break;
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
  private void propagatePromotion(XcodeProgram xcodeml,
                                  ClawTranslator translator)
      throws IllegalTransformationException
  {
    // Get all the assignment statements in the function definition
    XfunctionDefinition parentFctDef = _fctCall.findParentFunction();

    // Retrieve information of previous forward transformation in the same fct
    List<String> previouslyPromoted =
        Utility.convertToList(translator.hasElement(parentFctDef));

    List<Xnode> assignments = parentFctDef.matchAll(Xcode.FASSIGNSTATEMENT);
    List<DimensionDefinition> dimensions =
        TransformationHelper.findDimensions(_parentFctType);

    // Prepare the array index to be inserted in array references.
    List<Xnode> crt = new ArrayList<>();
    List<Xnode> empty = Collections.emptyList();
    for(DimensionDefinition dim : dimensions) {
      crt.add(dim.generateArrayIndex(xcodeml));
    }
    Collections.reverse(crt);
    List<List<Xnode>> induction = new ArrayList<>();
    List<List<Xnode>> emptyInd = new ArrayList<>();
    induction.add(crt);
    emptyInd.add(empty);

    for(Xnode assignment : assignments) {
      Xnode lhs = assignment.child(0);
      Xnode rhs = assignment.child(1);

      List<Xnode> varsInRhs = rhs.matchAll(Xcode.VAR);
      for(Xnode var : varsInRhs) {
        // Check if the assignment statement uses a promoted variable
        if(_promotedVar.contains(var.value())
            && var.matchAncestor(Xcode.FUNCTIONCALL) == null
            && lhs.opcode() == Xcode.FARRAYREF)
        {
          Xnode varInLhs = lhs.matchDescendant(Xcode.VAR);
          if(varInLhs == null) {
            throw new IllegalTransformationException("Unable to propagate " +
                "promotion. Internal error.", _claw.getPragma().lineNo());
          }

          XbasicType varType = xcodeml.getTypeTable().getBasicType(varInLhs);

          // Declare the induction variable if they are not present
          TransformationHelper.declareInductionVariables(dimensions,
              parentFctDef, xcodeml);

          // Generate the do statements and move the assignment statement in
          NestedDoStatement doStmt = new NestedDoStatement(dimensions, xcodeml);
          assignment.insertAfter(doStmt.getOuterStatement());
          doStmt.getInnerStatement().body().append(assignment);

          PromotionInfo promotionInfo;
          if(!previouslyPromoted.contains(varInLhs.value())) {
            // Perform the promotion on the variable
            promotionInfo = new PromotionInfo(varInLhs.value());
            //promotionInfo.setOverPosition();
            promotionInfo.setDimensions(dimensions);

            FieldTransform.promote(promotionInfo, parentFctDef, xcodeml);

            // TODO if #38 is implemented, the variable has to be put either in
            // TODO _promotedWithBeforeOver or _promotedWithAfterOver
            _promotedWithBeforeOver.add(varInLhs.value());
            _promotions.put(promotionInfo.getIdentifier(), promotionInfo);
          } else {
            promotionInfo = _promotions.get(varInLhs.value());
          }

          // Adapt the reference in the assignment statement
          TransformationHelper.adaptArrayReferences(_promotedWithBeforeOver, 0,
              assignment, _promotions, induction, emptyInd, emptyInd, xcodeml);
          TransformationHelper.adaptArrayReferences(_promotedWithMiddleOver, 0,
              assignment, _promotions, emptyInd, induction, emptyInd, xcodeml);
          TransformationHelper.adaptArrayReferences(_promotedWithAfterOver, 0,
              assignment, _promotions, emptyInd, emptyInd, induction, xcodeml);

          // If the array is a target, check if we have to promote a pointer
          if(!previouslyPromoted.contains(varInLhs.value())) {
            adaptPointer(varType, varInLhs.value(), parentFctDef, xcodeml,
                promotionInfo, dimensions);

            // TODO centralized info
            previouslyPromoted.add(varInLhs.value());
          }

          break;
          /* if one var in the rhs of the assignment statement was
           * promoted it's enough and we can switch to the next assignment
           * statement. */
        }
      }
    }

    translator.storeElement(parentFctDef, previouslyPromoted);
  }

  /**
   * Adapt potential pointer that are assigned from a promoted variable.
   *
   * @param varType     Type of the promoted variable.
   * @param fieldId     Name of the promoted variable.
   * @param fctDef      Function definition in which assignment statements are
   *                    checked.
   * @param xcodeml     Current XcodeML program unit.
   * @param pointeeInfo PromotionInformation about the promoted variable.
   * @param dimensions  List of dimensions to add.
   * @throws IllegalTransformationException If XcodeML modifications failed.
   */
  private void adaptPointer(XbasicType varType, String fieldId,
                            XfunctionDefinition fctDef, XcodeProgram xcodeml,
                            PromotionInfo pointeeInfo,
                            List<DimensionDefinition> dimensions)
      throws IllegalTransformationException
  {
    if(varType.isTarget()) {
      List<Xnode> pAssignments = fctDef.matchAll(Xcode.FPOINTERASSIGNSTATEMENT);
      for(Xnode pAssignment : pAssignments) {
        Xnode pointer = pAssignment.child(0);
        Xnode pointee = pAssignment.child(1);

        // Check if the pointer assignment has the promoted variable
        if(pointee.value().equals(fieldId)) {
          XbasicType pointerType = xcodeml.getTypeTable().getBasicType(pointer);
          XbasicType pointeeType = xcodeml.getTypeTable().
              getBasicType(pointeeInfo.getTargetType());

          // Check if their dimensions differ
          if(pointeeType.getDimensions() != pointerType.getDimensions()
              && !_promotions.containsKey(pointer.value()))
          {
            PromotionInfo promotionInfo = new PromotionInfo(pointer.value());
            if(_claw.hasOverClause()) {
              promotionInfo.setOverPosition(OverPosition.
                  fromList(_claw.getOverClauseValues().get(0)));
            }
            _promotions.put(pointer.value(), promotionInfo);
          }
        }
      }
    }
  }

  /**
   * @return Always false as independent transformation are applied one by one.
   * @see Transformation#canBeTransformedWith(XcodeProgram, Transformation)
   */
  @Override
  public boolean canBeTransformedWith(XcodeProgram xcodeml,
                                      Transformation other)
  {
    return false; // independent transformation
  }

  /**
   * Get the called fct name.
   *
   * @return Fct name.
   */
  public String getCalledFctName() {
    return _calledFctName;
  }

  /**
   * Get the parent fct name.
   *
   * @return Fct name.
   */
  public String getCallingFctName() {
    return _callingFctName;
  }
}
