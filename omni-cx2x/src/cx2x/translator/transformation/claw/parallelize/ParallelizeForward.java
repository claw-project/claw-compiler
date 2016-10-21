/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */
package cx2x.translator.transformation.claw.parallelize;

import cx2x.translator.common.NestedDoStatement;
import cx2x.translator.common.Utility;
import cx2x.translator.language.base.ClawLanguage;
import cx2x.translator.language.common.ClawDimension;
import cx2x.translator.language.common.OverPosition;
import cx2x.translator.language.helper.TransformationHelper;
import cx2x.translator.transformer.ClawTransformer;
import cx2x.translator.xnode.ClawAttr;
import cx2x.xcodeml.exception.IllegalTransformationException;
import cx2x.xcodeml.helper.XnodeUtil;
import cx2x.xcodeml.transformation.Transformation;
import cx2x.xcodeml.transformation.Transformer;
import cx2x.xcodeml.xnode.*;
import xcodeml.util.XmOption;

import java.util.*;

/**
 * The parallelize forward transformation applies the changes in the subroutine
 * signatures to function call and function in which the call is nested if
 * needed.
 * <p>
 * During the tranformation, a new "CLAW" XcodeML module file is generated
 * if the transformation has to be applied accross several file unit. This
 * file will be located in the same directory as the original XcodeML module
 * file and has the following naming structure: module_name.claw.xmod
 *
 * @author clementval
 */
public class ParallelizeForward extends Transformation {

  private final ClawLanguage _claw;
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
  private Xnode _innerDoStatement;
  private Xnode _outerDoStatement;
  private String _calledFctName;  // For topological sorting
  private String _callingFctName; // For topological sorting
  private boolean _isNestedInAssignement;


  /**
   * Constructs a new Parallelize transformation triggered from a specific
   * pragma.
   *
   * @param directive The directive that triggered the define transformation.
   */
  public ParallelizeForward(ClawLanguage directive) {
    super(directive);
    _claw = directive; // Keep information about the claw directive here
    _promotedVar = new ArrayList<>();
    _promotedWithBeforeOver = new ArrayList<>();
    _promotedWithAfterOver = new ArrayList<>();
    _promotedWithMiddleOver = new ArrayList<>();
    _promotions = new HashMap<>();
    _fctCallMapping = new HashMap<>();
  }

  @Override
  public boolean analyze(XcodeProgram xcodeml, Transformer transformer) {
    Xnode next = XnodeUtil.getNextSibling(_claw.getPragma());
    if(next == null) {
      xcodeml.addError("Directive is not followed by a valid statement.",
          _claw.getPragma().getLineNo());
      return false;
    }
    if(next.opcode() == Xcode.EXPRSTATEMENT
        || next.opcode() == Xcode.FASSIGNSTATEMENT)
    {
      _isNestedInAssignement = next.opcode() == Xcode.FASSIGNSTATEMENT;
      _fctCall = next.matchSeq(Xcode.FUNCTIONCALL);
      if(_fctCall != null) {
        return analyzeForward(xcodeml);
      }
    } else if(next.opcode() == Xcode.FDOSTATEMENT) {
      _outerDoStatement = next;
      return analyzeForwardWithDo(xcodeml, next);
    }
    xcodeml.addError("Directive is not followed by a valid statement.",
        _claw.getPragma().getLineNo());
    return false;
  }

  /**
   * Analyze the directive when it is used just before a do statement.
   *
   * @param xcodeml Current XcodeML file unit.
   * @param doStmt  The do statement following the pragma.
   * @return True if the analysis succeed. False otherwise.
   */
  private boolean analyzeForwardWithDo(XcodeProgram xcodeml, Xnode doStmt) {
    _flatten = true;
    if(doStmt == null) {
      xcodeml.addError("Directive is not followed by do statement.",
          _claw.getPragma().getLineNo());
      return false;
    }

    // Try to locate the fct call inside of the do statements. Can be nested.
    return analyzeNestedDoStmts(xcodeml, doStmt);
  }

  /**
   * Recursively analyze nested do statements in order to matchSeq the call
   * statements.
   *
   * @param xcodeml Current XcodeML file unit.
   * @param doStmt  First do statement to start the analyzis.
   * @return True if the analysis succed. False otehrwise.
   */
  private boolean analyzeNestedDoStmts(XcodeProgram xcodeml, Xnode doStmt) {
    _innerDoStatement = doStmt;
    Xnode body = doStmt.matchSeq(Xcode.BODY);
    if(body == null) {
      xcodeml.addError("Cannot locate function call.",
          _claw.getPragma().getLineNo());
      return false;
    }
    for(Xnode n : body.children()) {
      if(n.opcode() == Xcode.FDOSTATEMENT) {
        return analyzeNestedDoStmts(xcodeml, n);
      } else if(n.opcode() != Xcode.FPRAGMASTATEMENT
          && n.opcode() != Xcode.EXPRSTATEMENT)
      {
        xcodeml.addError("Only pragmas, comments and function calls allowed " +
                "in the do statements.",
            _claw.getPragma().getLineNo());
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
    xcodeml.addError("Function call not found.", _claw.getPragma().getLineNo());
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
          _claw.getPragma().getLineNo());
      return false;
    }

    detectParameterMapping(_fctCall);

    _calledFctName = _fctCall.matchSeq(Xcode.NAME).getValue();

    XfunctionDefinition fctDef = XnodeUtil.findFunctionDefinition(
        xcodeml.getGlobalDeclarationsTable(), _calledFctName);
    XfunctionDefinition parentFctDef =
        XnodeUtil.findParentFunction(_claw.getPragma());
    if(parentFctDef == null) {
      xcodeml.addError("Parellilize directive is not nested in a " +
          "function/subroutine.", _claw.getPragma().getLineNo());
      return false;
    }

    XmoduleDefinition parentModule = XnodeUtil.findParentModule(parentFctDef);

    String fctType = _fctCall.matchSeq(Xcode.NAME).getAttribute(Xattr.TYPE);
    if(fctType.startsWith(Xtype.PREFIX_PROCEDURE)) {
      /* If type is a FbasicType element for a type-bound procedure, we have to
       * matchSeq the correct function in the typeTable.
       * TODO if there is a rename.
       * TODO generic call */
      Xid id = parentModule.getSymbolTable().get(_calledFctName);
      if(id == null) {
        List<Xdecl> uses = XnodeUtil.getAllUse(parentFctDef);
        uses.addAll(XnodeUtil.getAllUse(parentModule));
        if(!findInModule(uses)) {
          xcodeml.addError("Function definition not found in module ",
              _claw.getPragma().getLineNo());
          return false;
        }
      } else {
        _fctType = (XfunctionType) xcodeml.getTypeTable().get(id.getType());
      }
    } else {
      Xtype rawType = xcodeml.getTypeTable().get(fctType);
      if(rawType instanceof XfunctionType) {
        _fctType = (XfunctionType) rawType;
      } else {
        xcodeml.addError("Unsupported type of XcodeML/F element for the function "
            + _calledFctName, _claw.getPragma().getLineNo());
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
        List<Xdecl> uses = XnodeUtil.getAllUse(parentFctDef);
        uses.addAll(XnodeUtil.getAllUse(parentModule));
        if(!findInModule(uses)) {
          xcodeml.addError("Function definition not found in module ",
              _claw.getPragma().getLineNo());
          return false;
        }
      } else {
        _fctType = (XfunctionType) xcodeml.getTypeTable().get(id.getType());
        if(_fctType == null) {
          xcodeml.addError(
              "Called function cannot be found in the same module ",
              _claw.getPragma().getLineNo());
          return false;
        }
      }
    }
    // end of workaround

    _callingFctName = parentFctDef.getName().getValue();
    if(_fctType != null && fctDef != null) {
      _localFct = true;
    } else {

      // Has been found already
      if(_fctType != null && _calledFctName == null) {
        return true;
      }

      // Get all the use statements in the fct and module definitions
      List<Xdecl> uses = XnodeUtil.getAllUse(parentFctDef);
      uses.addAll(XnodeUtil.getAllUse(parentModule));

      // Try to locate the fct in the modules defined in use statements
      if(findInModule(uses)) {
        return true;
      }

      xcodeml.addError("Function signature not found in the current module.",
          _claw.getPragma().getLineNo());
      return false;
    }


    return true;
  }

  /**
   * Get all the mapping between local variable and parameter names in the
   * function call.
   *
   * @param fctCall Fonction call to be analyzed.
   */
  private void detectParameterMapping(Xnode fctCall) {
    if(fctCall == null || fctCall.opcode() != Xcode.FUNCTIONCALL) {
      return;
    }
    for(Xnode arg : _fctCall.matchSeq(Xcode.ARGUMENTS).children()) {
      if(arg.opcode() == Xcode.NAMEDVALUE) {
        String original_name = arg.getAttribute(Xattr.NAME);
        Xnode target_var = XnodeUtil.find(Xcode.VAR, arg, true);
        if(target_var != null) {
          _fctCallMapping.put(original_name, target_var.getValue());

          if(XmOption.isDebugOutput()) {
            System.out.println("Fct parameter mapping: original_name=" +
                original_name + " target_name=" + target_var.getValue());
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
  private boolean findInModule(List<Xdecl> useDecls) {
    // TODO handle rename
    for(Xdecl d : useDecls) {

      // Check whether a CLAW file is available.
      _mod = TransformationHelper.
          locateClawModuleFile(d.getAttribute(Xattr.NAME));

      if(_mod != null) {

        // debug information
        if(XmOption.isDebugOutput()) {
          System.out.println("Reading CLAW module file: " + _mod.getFullPath());
        }

        if(_mod.getIdentifiers().contains(_calledFctName)) {
          String type = _mod.getIdentifiers().get(_calledFctName).
              getAttribute(Xattr.TYPE);
          _fctType = (XfunctionType) _mod.getTypeTable().get(type);
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
  public void transform(XcodeProgram xcodeml, Transformer transformer,
                        Transformation other) throws Exception
  {
    if(_flatten) {
      transformFlatten(xcodeml, transformer);
    } else {
      transformStd(xcodeml, transformer);
    }

    // Delete pragma
    _claw.getPragma().delete();
  }

  /**
   * Do the flatten transformation for the forward directive. This
   * transformation adapt the function call nested in the do statements and
   * removes those do statements. The containing subroutine is not adapted.
   *
   * @param xcodeml     Current XcodeML file unit.
   * @param transformer Current transformer.
   * @throws Exception If something goes wrong.
   */
  private void transformFlatten(XcodeProgram xcodeml, Transformer transformer)
      throws Exception
  {
    XnodeUtil.extractBody(_innerDoStatement, _outerDoStatement);
    _outerDoStatement.delete();
    transformStd(xcodeml, transformer);
  }

  /**
   * Do the standard transformation for the forward directive. This
   * transformation adapt the function call and replicates any necessary changes
   * to the containing subroutine.
   *
   * @param xcodeml     Current XcodeML file unit.
   * @param transformer Current transformer.
   * @throws Exception If something goes wrong.
   */
  private void transformStd(XcodeProgram xcodeml, Transformer transformer)
      throws Exception
  {
    XfunctionDefinition fDef = XnodeUtil.findParentFunction(_claw.getPragma());
    if(fDef == null) {
      throw new IllegalTransformationException("Parallelize directive is not " +
          "nested in a function/subroutine.", _claw.getPragma().getLineNo());
    }
    _parentFctType = (XfunctionType) xcodeml.getTypeTable().
        get(fDef.getName().getAttribute(Xattr.TYPE));

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
    if(params.get(0).getAttribute(Xattr.TYPE).startsWith(Xtype.PREFIX_STRUCT)
        && _fctCall.matchSeq(Xcode.NAME).hasAttribute(Xattr.DATAREF))
    {
      argOffset = 1;
    }

    // 1. Adapt function call with potential new arguments
    for(int i = 0; i < params.size(); i++) {
      Xnode p = params.get(i);
      String var = p.getValue();
      String type;

      XbasicType paramType =
          (XbasicType) xcodeml.getTypeTable().get(p.getAttribute(Xattr.TYPE));

      if(!p.getBooleanAttribute(ClawAttr.IS_CLAW.toString())) {
        continue;
      }

      if(!fDef.getSymbolTable().contains(var)) {
        if(_flatten && !paramType.getBooleanAttribute(Xattr.IS_OPTIONAL)) {
          throw new IllegalTransformationException("Variable " + var + " must" +
              " be locally defined where the last call to parallelize if made.",
              _claw.getPragma().getLineNo());
        }
        // Size variable have to be declared
        XbasicType intTypeIntentIn = XnodeUtil.createBasicType(xcodeml,
            xcodeml.getTypeTable().generateIntegerTypeHash(),
            Xname.TYPE_F_INT, Xintent.IN);
        xcodeml.getTypeTable().add(intTypeIntentIn);
        XnodeUtil.createIdAndDecl(var, intTypeIntentIn.getType(),
            Xname.SCLASS_F_PARAM, fDef, xcodeml);
        type = intTypeIntentIn.getType();
        XnodeUtil.createAndAddParam(xcodeml, var, type, _parentFctType);
      } else {

        // Var exists already. Add to the parameters if not here.
        type = fDef.getSymbolTable().get(var).getType();

        /* If flatten mode, we do not add extra parameters to the function
         * definition */
        if(!_flatten) {
          XnodeUtil.
              createAndAddParamIfNotExists(xcodeml, var, type, _parentFctType);
        }
      }

      // Add variable in the function call before the optional parameters
      Xnode arg = XnodeUtil.createNamedValue(var, xcodeml);
      Xnode namedValVar = XnodeUtil.createVar(type, var, Xscope.LOCAL, xcodeml);
      arg.append(namedValVar, false);
      Xnode arguments = _fctCall.matchSeq(Xcode.ARGUMENTS);
      Xnode hook = arguments.child((i - 1) - argOffset);
      XnodeUtil.insertAfter(hook, arg);
    }

    // In flatten mode, arguments are demoted if needed.
    if(_flatten) {
      Xnode arguments = _fctCall.matchSeq(Xcode.ARGUMENTS);
      for(Xnode arg : arguments.children()) {
        if(arg.opcode() == Xcode.FARRAYREF && arg.matchAny(
            Arrays.asList(Xcode.INDEXRANGE, Xcode.ARRAYINDEX)) != null)
        {
          Xnode var = arg.matchSeq(Xcode.VARREF, Xcode.VAR);
          if(var != null) {
            XnodeUtil.insertAfter(arg, var.cloneNode());
            arg.delete();
          }
        }
      }
    } else {
      // 2. Adapt function/subroutine in which the function call is nested
      for(Xnode pBase : _fctType.getParams().getAll()) {
        String original_param = pBase.getValue();
        if(_fctCallMapping.containsKey(original_param)) {
          original_param = _fctCallMapping.get(original_param);
        }

        Xnode pUpdate = null;
        for(Xnode param : _parentFctType.getParams().getAll()) {
          if(original_param.equals(param.getValue())) {
            pUpdate = param;
          }
        }

        if(pUpdate == null) { // field is not a parameter but maybe out field
          Xdecl d = fDef.getDeclarationTable().get(original_param);
          if(d != null) {
            pUpdate = d.matchSeq(Xcode.NAME);
          }
          // TODO handle deferred shape
        }

        if(pUpdate != null) {

          if(pUpdate.getAttribute(Xattr.TYPE) == null
              || XnodeUtil.isBuiltInType(pUpdate.getAttribute(Xattr.TYPE)))
          {
            continue;
          }

          XbasicType typeBase = (_localFct) ? (XbasicType)
              xcodeml.getTypeTable().get(pBase.getAttribute(Xattr.TYPE)) :
              (XbasicType) _mod.getTypeTable().
                  get(pBase.getAttribute(Xattr.TYPE));
          XbasicType typeToUpdate = (XbasicType) xcodeml.getTypeTable().
              get(pUpdate.getAttribute(Xattr.TYPE));
          int targetDim = typeBase.getDimensions();
          int baseDim = typeToUpdate.getDimensions();


          // Types have different dimensions
          if(typeBase.getDimensions() > typeToUpdate.getDimensions()) {

            // TODO check intent rules
            /*if(!isParam && typeToUpdate.getIntent() != Xintent.OUT){
              continue;
            }*/

            List<ClawDimension> dimensions =
                TransformationHelper.findDimensions(_fctType);
            OverPosition overPos = OverPosition.fromString(
                pBase.getAttribute(ClawAttr.OVER.toString()));

            String type = _localFct ?
                XnodeUtil.duplicateWithDimension(typeBase, typeToUpdate,
                    xcodeml, xcodeml, overPos, dimensions)
                : XnodeUtil.duplicateWithDimension(typeBase, typeToUpdate,
                xcodeml, _mod, overPos, dimensions);

            pUpdate.setAttribute(Xattr.TYPE, type);

            Xid id = fDef.getSymbolTable().get(original_param);
            if(id != null) {
              id.setAttribute(Xattr.TYPE, type);
            }
            Xdecl varDecl = fDef.getDeclarationTable().get(original_param);
            if(varDecl != null) {
              varDecl.matchSeq(Xcode.NAME).setAttribute(Xattr.TYPE, type);
            }

            _promotedVar.add(original_param);


            addPromotedVar(original_param, overPos);

            _promotions.put(original_param, new PromotionInfo(
                pBase.getValue(), baseDim, targetDim, type));
          }
        }


      }

      if(!_parentFctType.getBooleanAttribute(Xattr.IS_PRIVATE)) {
        // 3. Replicate the change in a potential module file
        XmoduleDefinition modDef = XnodeUtil.findParentModule(fDef);
        TransformationHelper.updateModuleSignature(xcodeml, fDef,
            _parentFctType, modDef, _claw, transformer, false);
      } else if(_fctCall.matchSeq(Xcode.NAME).hasAttribute(Xattr.DATAREF)) {
        /* The function/subroutine is private but accessible through the type
         * as a type-bound procedure. In this case, the function is not in the
         * type table of the .xmod file. We need to insert it first and then
         * we can update it. */
        XmoduleDefinition modDef = XnodeUtil.findParentModule(fDef);
        TransformationHelper.updateModuleSignature(xcodeml, fDef,
            _parentFctType, modDef, _claw, transformer, true);
      }
    }

    updateResultVar(xcodeml);

    propagatePromotion(xcodeml, (ClawTransformer) transformer);
  }

  /**
   * Apply promotion to the result return variable of a foward call.
   *
   * @param xcodeml Current XcodeML program unit.
   * @throws IllegalTransformationException If XcodeML transformation cannot be
   *                                        done.
   */
  private void updateResultVar(XcodeProgram xcodeml)
      throws IllegalTransformationException
  {
    if(_isNestedInAssignement) {
      Xnode assignment = XnodeUtil.getNextSibling(_claw.getPragma());
      if(assignment == null
          || !_fctType.hasAttribute(ClawAttr.OVER.toString()))
      {
        return;
      }

      OverPosition overPos = OverPosition.fromString(
          _fctType.getAttribute(ClawAttr.OVER.toString()));

      Xnode lhs = assignment.child(0);
      // TODO handle the case when the array ref is a var directly
      Xnode varInLhs = XnodeUtil.find(Xcode.VAR, lhs, true);

      List<ClawDimension> dimensions =
          TransformationHelper.findDimensions(_parentFctType);
      XfunctionDefinition parentFctDef =
          XnodeUtil.findParentFunction(_fctCall);

      XbasicType varType = (XbasicType)
          xcodeml.getTypeTable().get(varInLhs.getAttribute(Xattr.TYPE));

      PromotionInfo promotionInfo;
      if(!_promotions.containsKey(varInLhs.getValue())) {
        // Perform the promotion on the variable
        promotionInfo = TransformationHelper.promoteField(
            varInLhs.getValue(), true, true, 0, 0, parentFctDef,
            _parentFctType, dimensions, _claw, xcodeml, overPos);
        _promotions.put(varInLhs.getValue(), promotionInfo);

        addPromotedVar(varInLhs.getValue(), overPos);

      } else {
        promotionInfo = _promotions.get(varInLhs.getValue());
      }

      // Adapte array index to reflect the new return type
      if(lhs.opcode() == Xcode.FARRAYREF) {
        for(int i = 0; i < promotionInfo.diffDimension(); ++i) {
          Xnode indexRange = XnodeUtil.createEmptyAssumedShaped(xcodeml);
          lhs.append(indexRange, false);
        }
      } else if(lhs.opcode() == Xcode.VAR) {
        // TODO avoid array var without colon notation
          /* throw new IllegalTransformationException("Use the colon notation "
              + "for the return variable. This notation is not supported." +
              _claw.getPragma().getValue()); */
      } else {
        throw new IllegalTransformationException("Unsupported return " +
            "variable for promotion.", _claw.getPragma().getLineNo());
      }

      // If the array is a target, check if we have to promote a pointer
      adpatPointer(varType, varInLhs.getValue(), parentFctDef, xcodeml,
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
   * Propagate possible promotion in assignements statements in the parent
   * subroutine of the function call.
   *
   * @param xcodeml     Current XcodeML program unit.
   * @param transformer Current transformer to store information between
   *                    transformation.
   */
  private void propagatePromotion(XcodeProgram xcodeml,
                                  ClawTransformer transformer)
      throws IllegalTransformationException
  {
    // Get all the assignement statements in the function definiton
    XfunctionDefinition parentFctDef = XnodeUtil.findParentFunction(_fctCall);

    // Retrieve information of previous forward transformation in the same fct
    List<String> previouslyPromoted =
        Utility.convertToList(transformer.hasElement(parentFctDef));

    List<Xnode> assignments =
        XnodeUtil.findAll(Xcode.FASSIGNSTATEMENT, parentFctDef);
    List<ClawDimension> dimensions =
        TransformationHelper.findDimensions(_parentFctType);


    // Prepare the array index to be inserted in array references.
    List<Xnode> crt = new ArrayList<>();
    List<Xnode> empty = Collections.emptyList();
    for(ClawDimension dim : dimensions) {
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

      List<Xnode> varsInRhs = XnodeUtil.findAll(Xcode.VAR, rhs);
      for(Xnode var : varsInRhs) {
        // Check if the assignement statement uses a promoted variable
        if(_promotedVar.contains(var.getValue())
            && XnodeUtil.findParent(Xcode.FUNCTIONCALL, var) == null
            && lhs.opcode() == Xcode.FARRAYREF)
        {
          Xnode varInLhs = XnodeUtil.find(Xcode.VAR, lhs, true);
          if(varInLhs == null) {
            throw new IllegalTransformationException("Unable to propagate " +
                "promotion. Internal error.", _claw.getPragma().getLineNo());
          }

          XbasicType varType = (XbasicType)
              xcodeml.getTypeTable().get(varInLhs.getAttribute(Xattr.TYPE));

          // Declare the induction variable if they are not present
          TransformationHelper.declareInductionVariables(dimensions,
              parentFctDef, xcodeml);

          // Generate the do statements and move the assignement statement in
          NestedDoStatement doStmt = new NestedDoStatement(dimensions, xcodeml);
          XnodeUtil.insertAfter(assignment, doStmt.getOuterStatement());
          doStmt.getInnerStatement().body().
              append(assignment, false);

          PromotionInfo promotionInfo;
          if(!previouslyPromoted.contains(varInLhs.getValue().toLowerCase())) {
            // Perform the promotion on the variable
            promotionInfo = TransformationHelper.promoteField(
                varInLhs.getValue(), true, true, 0, 0, parentFctDef,
                _parentFctType, dimensions, _claw, xcodeml, null);
            _promotions.put(varInLhs.getValue(), promotionInfo);

            // TODO if #38 is implemented, the varibale has to be put either in
            // TODO _promotedWithBeforeOver or _promotedWithAfterOver
            _promotedWithBeforeOver.add(varInLhs.getValue());
          } else {
            promotionInfo = _promotions.get(varInLhs.getValue());
          }

          // Adapt the reference in the assignement statement
          TransformationHelper.adaptArrayReferences(_promotedWithBeforeOver, 0,
              assignment, _promotions, induction, emptyInd, emptyInd, xcodeml);
          TransformationHelper.adaptArrayReferences(_promotedWithMiddleOver, 0,
              assignment, _promotions, emptyInd, induction, emptyInd, xcodeml);
          TransformationHelper.adaptArrayReferences(_promotedWithAfterOver, 0,
              assignment, _promotions, emptyInd, emptyInd, induction, xcodeml);

          // If the array is a target, check if we have to promote a pointer
          if(!previouslyPromoted.contains(varInLhs.getValue().toLowerCase())) {
            adpatPointer(varType, varInLhs.getValue(), parentFctDef, xcodeml,
                promotionInfo, dimensions);

            // TODO centralized info
            previouslyPromoted.add(varInLhs.getValue().toLowerCase());
          }

          break;
          /* if one var in the rhs of the assignement statement was
           * promoted it's enough and we can switch to the next assignement
           * statement. */
        }
      }
    }

    transformer.storeElement(parentFctDef, previouslyPromoted);
  }

  /**
   * Adapt potential pointer that are assigned from a promoted variable.
   *
   * @param varType     Type of the promoted variable.
   * @param fieldId     Name of the promoted variable.
   * @param fctDef      Function definition in which assignement statements are
   *                    checked.
   * @param xcodeml     Current XcodeML program unit.
   * @param pointeeInfo PromotionInformation about the promoted variable.
   * @param dimensions  List of dimensions to add.
   * @throws IllegalTransformationException If XcodeML modifications failed.
   */
  private void adpatPointer(XbasicType varType, String fieldId,
                            XfunctionDefinition fctDef, XcodeProgram xcodeml,
                            PromotionInfo pointeeInfo,
                            List<ClawDimension> dimensions)
      throws IllegalTransformationException
  {
    if(varType.isTarget()) {
      List<Xnode> pAssignments =
          XnodeUtil.findAll(Xcode.FPOINTERASSIGNSTATEMENT, fctDef);
      for(Xnode pAssignment : pAssignments) {
        Xnode pointer = pAssignment.child(0);
        Xnode pointee = pAssignment.child(1);

        // Check if the pointer assignment has the promoted variable
        if(pointee.getValue().toLowerCase().
            equals(fieldId.toLowerCase()))
        {
          XbasicType pointerType = (XbasicType) xcodeml.getTypeTable().
              get(pointer.getAttribute(Xattr.TYPE));
          XbasicType pointeeType = (XbasicType) xcodeml.getTypeTable().
              get(pointeeInfo.getTargetType());

          // Check if their dimensions differ
          if(pointeeType.getDimensions() != pointerType.getDimensions()
              && !_promotions.containsKey(pointer.getValue()))
          {
            PromotionInfo promotionInfo = TransformationHelper.promoteField(
                pointer.getValue(), true, true, 0, dimensions.size(),
                fctDef, _parentFctType, dimensions, _claw, xcodeml, null);
            _promotions.put(pointer.getValue(), promotionInfo);
          }
        }
      }
    }
  }

  @Override
  public boolean canBeTransformedWith(Transformation other) {
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
