/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */
package cx2x.translator.transformation.claw.parallelize;

import cx2x.translator.language.ClawLanguage;
import cx2x.translator.language.helper.TransformationHelper;
import cx2x.xcodeml.exception.IllegalTransformationException;
import cx2x.xcodeml.helper.XnodeUtil;
import cx2x.xcodeml.transformation.Transformation;
import cx2x.xcodeml.transformation.Transformer;
import cx2x.xcodeml.xnode.*;
import xcodeml.util.XmOption;

import java.util.List;

/**
 * The parallelize forward transformation applies the changes in the subroutine
 * signatures to function call and function in which the call is nested if
 * needed.
 *
 * During the tranformation, a new "CLAW" XcodeML module file is generated
 * if the transformation has to be applied accross several file unit. This
 * file will be located in the same directory as the original XcodeML module
 * file and has the following naming structure: module_name.claw.xmod
 *
 * @author clementval
 */
public class ParallelizeForward extends Transformation {

  private final ClawLanguage _claw;
  private Xnode _fctCall;
  private XfunctionType _fctType;
  private Xmod _mod = null;
  private boolean _localFct = false;
  private boolean _flatten = false;

  private Xnode _innerDoStatement;
  private Xnode _outerDoStatement;

  private String _calledFctName;  // For topological sorting
  private String _callingFctName; // For topological sorting


  /**
   * Constructs a new Parallelize transformation triggered from a specific
   * pragma.
   * @param directive The directive that triggered the define transformation.
   */
  public ParallelizeForward(ClawLanguage directive) {
    super(directive);
    _claw = directive; // Keep information about the claw directive here
  }

  @Override
  public boolean analyze(XcodeProgram xcodeml, Transformer transformer) {
    Xnode next = XnodeUtil.getNextSibling(_claw.getPragma());
    if(next == null){
      xcodeml.addError("Directive is not followed by a valid statement.",
          _claw.getPragma().getLineNo());
      return false;
    }
    if(next.Opcode() == Xcode.EXPRSTATEMENT
        || next.Opcode() == Xcode.FASSIGNSTATEMENT)
    {
      _fctCall = next.find(Xcode.FUNCTIONCALL);
      if(_fctCall != null){
        return analyzeForward(xcodeml);
      }
    } else if (next.Opcode() == Xcode.FDOSTATEMENT) {
      _outerDoStatement = next;
      return analyzeForwardWithDo(xcodeml, next);
    }
    xcodeml.addError("Directive is not followed by a valid statement.",
        _claw.getPragma().getLineNo());
    return false;
  }

  /**
   * Analyze the directive when it is used just before a do statement.
   * @param xcodeml Current XcodeML file unit.
   * @param doStmt  The do statement following the pragma.
   * @return True if the analysis succeed. False otherwise.
   */
  private boolean analyzeForwardWithDo(XcodeProgram xcodeml, Xnode doStmt){
    _flatten = true;
    if(doStmt == null){
      xcodeml.addError("Directive is not followed by do statement.",
          _claw.getPragma().getLineNo());
      return false;
    }

    // Try to locate the fct call inside of the do statements. Can be nested.
    return analyzeNestedDoStmts(xcodeml, doStmt);
  }

  /**
   * Recursively analyze nested do statements in order to find the call
   * statements.
   * @param xcodeml Current XcodeML file unit.
   * @param doStmt  First do statement to start the analyzis.
   * @return True if the analysis succed. False otehrwise.
   */
  private boolean analyzeNestedDoStmts(XcodeProgram xcodeml, Xnode doStmt){
    _innerDoStatement = doStmt;
    Xnode body = doStmt.find(Xcode.BODY);
    if(body == null){
      xcodeml.addError("Cannot locate function call.",
          _claw.getPragma().getLineNo());
      return false;
    }
    for(Xnode n : body.getChildren()){
      if(n.Opcode() == Xcode.FDOSTATEMENT){
        return analyzeNestedDoStmts(xcodeml, n);
      } else if(n.Opcode() != Xcode.FPRAGMASTATEMENT
          && n.Opcode() != Xcode.EXPRSTATEMENT)
      {
        xcodeml.addError("Only pragmas, comments and function calls allowed " +
                "in the do statements.",
            _claw.getPragma().getLineNo());
        return false;
      } else if (n.Opcode() == Xcode.EXPRSTATEMENT
          || n.Opcode() == Xcode.FASSIGNSTATEMENT)
      {
        _fctCall = n.find(Xcode.FUNCTIONCALL);
        if(_fctCall != null){
          return analyzeForward(xcodeml);
        }
      }
    }
    xcodeml.addError("Function call not found.", _claw.getPragma().getLineNo());
    return false;
  }

  /**
   * Analyze the directive when it is used just before a function call.
   * @param xcodeml Current XcodeML file unit.
   * @return True if the analysis succeed. False otherwise.
   */
  private boolean analyzeForward(XcodeProgram xcodeml){
    if(_fctCall == null){
      xcodeml.addError("Directive is not followed by a fct call.",
          _claw.getPragma().getLineNo());
      return false;
    }
    _calledFctName = _fctCall.find(Xcode.NAME).getValue();







    XfunctionDefinition fctDef = XnodeUtil.findFunctionDefinition(
        xcodeml.getGlobalDeclarationsTable(), _calledFctName);
    XfunctionDefinition parentFctDef =
        XnodeUtil.findParentFunction(_claw.getPragma());
    if(parentFctDef == null){
      xcodeml.addError("Parellilize directive is not nested in a " +
          "function/subroutine.", _claw.getPragma().getLineNo());
      return false;
    }


    XmoduleDefinition parentModule = XnodeUtil.findParentModule(parentFctDef);

    

    /* Workaround for a bug in OMNI Compiler. Look at test case
     * claw/abstraction10. In this test case, the XcodeML/F intermediate
     * representation for the function call points to a FfunctionType element
     * with no parameters. Thus, we have to find the correct FfunctionType
     * for the same function/subroutine with the same name in the module
     * symbol table.
     */
    // Check that the arguments of the FfunctionType match with functionCall
    if(_fctType.getParameterNb() == 0){
      // If not, try to find the correct FfunctionType in the module definitions
      Xid id = parentModule.getSymbolTable().get(_calledFctName);
      _fctType = (XfunctionType)xcodeml.getTypeTable().get(id.getType());
      if(_fctType == null){
        xcodeml.addError("Called function cannot be found in the same module ",
            _claw.getPragma().getLineNo());
        return false;
      }
    }
    // end of workaround

    _callingFctName = parentFctDef.getName().getValue();
    if(_fctType != null && fctDef != null){
      _localFct = true;
    } else {
      List<Xdecl> localScopeUsesStmt = XnodeUtil.getAllUse(parentFctDef);
      List<Xdecl> moduleScoptUsesStmt =
          XnodeUtil.getAllUse(parentModule);

      if(findInModule(localScopeUsesStmt) || findInModule(moduleScoptUsesStmt)){
        return true;
      }

      xcodeml.addError("Function signature not found in the current module.",
          _claw.getPragma().getLineNo());
      return false;
    }
    return true;
  }

  /**
   * Find a function in modules.
   * @param useDecls List of all USE statement declarations available for
   *                 search.
   * @return True if the function was found. False otherwise.
   */
  private boolean findInModule(List<Xdecl> useDecls){
    for(Xdecl d : useDecls){

      // Check whether a CLAW file is available.
      _mod = TransformationHelper.
          locateClawModuleFile(d.getAttribute(Xattr.NAME));

      if(_mod != null){

        // debug information
        if(XmOption.isDebugOutput()){
          System.out.println("Reading CLAW module file: " + _mod.getFullPath());
        }

        if(_mod.getIdentifiers().contains(_calledFctName)){
          String type = _mod.getIdentifiers().get(_calledFctName).
              getAttribute(Xattr.TYPE);
          _fctType = (XfunctionType) _mod.getTypeTable().get(type);
          if(_fctType != null){
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
    if(_flatten){
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
   * @param xcodeml     Current XcodeML file unit.
   * @param transformer Current transformer.
   * @throws Exception If something goes wrong.
   */
  private void transformStd(XcodeProgram xcodeml, Transformer transformer)
      throws Exception
  {
    XfunctionDefinition fDef = XnodeUtil.findParentFunction(_claw.getPragma());
    if(fDef == null){
      throw new IllegalTransformationException("Parallelize directive is not " +
          "nested in a function/subroutine.", _claw.getPragma().getLineNo());
    }
    XfunctionType parentFctType = (XfunctionType)xcodeml.getTypeTable().
        get(fDef.getName().getAttribute(Xattr.TYPE));

    List<Xnode> params = _fctType.getParams().getAll();
    List<Xnode> args = _fctCall.find(Xcode.ARGUMENTS).getChildren();

    // 1. Adapt function call with potential new arguments
    for(int i = args.size(); i < params.size(); i++){
      Xnode p = params.get(i);
      String var = p.getValue();
      String type;

      if(!fDef.getSymbolTable().contains(var)){
        if(_flatten){
          throw new IllegalTransformationException("Variable " + var + " must" +
              " be locally defined where the last call to parallelize if made.",
              _claw.getPragma().getLineNo());
        }
        // Size variable have to be declared
        XbasicType intTypeIntentIn = XnodeUtil.createBasicType(xcodeml,
            xcodeml.getTypeTable().generateIntegerTypeHash(),
            Xname.TYPE_F_INT, Xintent.IN);
        xcodeml.getTypeTable().add(intTypeIntentIn);
        XnodeUtil.createIdAndDecl(var,
            intTypeIntentIn.getType(), Xname.SCLASS_F_PARAM, fDef, xcodeml);
        type = intTypeIntentIn.getType();
        XnodeUtil.createAndAddParam(xcodeml, var, type, parentFctType);
      } else {
        // Var exists already. Add to the parameters if not here.
        type = fDef.getSymbolTable().get(var).getType();

        /* If flatten mode, we do not add extra parameters to the function
         * definition */
        if(!_flatten) {
          XnodeUtil.
              createAndAddParamIfNotExists(xcodeml, var, type, parentFctType);
        }
      }

      // Add variable in the function call
      Xnode arg = XnodeUtil.createNamedValue(var, xcodeml);
      Xnode namedValVar = XnodeUtil.createVar(type, var, Xscope.LOCAL, xcodeml);
      arg.appendToChildren(namedValVar, false);
      _fctCall.find(Xcode.ARGUMENTS).appendToChildren(arg, false);
    }

    // In flatten mode, arguments are demoted if needed.
    if(_flatten){
      Xnode arguments = _fctCall.find(Xcode.ARGUMENTS);
      for(Xnode arg : arguments.getChildren()){
        if(arg.Opcode() == Xcode.FARRAYREF
            && arg.find(Xcode.INDEXRANGE) != null)
        {
          Xnode var = arg.find(Xcode.VARREF, Xcode.VAR);
          if(var != null){
            XnodeUtil.insertAfter(arg, var.cloneObject());
            arg.delete();
          }
        }
      }
    } else {
      // 2. Adapt function/subroutine in which the function call is nested
      for(Xnode pBase : _fctType.getParams().getAll()){
        for(Xnode pUpdate : parentFctType.getParams().getAll()){
          if(pBase.getValue().equals(pUpdate.getValue())){
            XbasicType typeBase = (_localFct) ? (XbasicType)
                xcodeml.getTypeTable().get(pBase.getAttribute(Xattr.TYPE)) :
                (XbasicType) _mod.getTypeTable().
                    get(pBase.getAttribute(Xattr.TYPE));
            XbasicType typeToUpdate = (XbasicType)xcodeml.getTypeTable().
                get(pUpdate.getAttribute(Xattr.TYPE));

            // Types have different dimensions
            if(typeBase.getDimensions() > typeToUpdate.getDimensions()){
              String type = XnodeUtil.duplicateWithDimension(typeBase,
                  typeToUpdate, xcodeml);
              pUpdate.setAttribute(Xattr.TYPE, type);

              Xid id = fDef.getSymbolTable().get(pBase.getValue());
              if(id != null){
                id.setAttribute(Xattr.TYPE, type);
              }
              Xdecl varDecl = fDef.getDeclarationTable().get(pBase.getValue());
              if(varDecl != null){
                varDecl.find(Xcode.NAME).setAttribute(Xattr.TYPE, type);
              }
            }
          }
        }
      }

      if(!parentFctType.getBooleanAttribute(Xattr.IS_PRIVATE)){
        // 3. Replicate the change in a potential module file
        XmoduleDefinition modDef = XnodeUtil.findParentModule(fDef);
        XnodeUtil.updateModuleSignature(xcodeml, fDef, parentFctType, modDef,
            _claw, transformer);
      }
    }

  }

  @Override
  public boolean canBeTransformedWith(Transformation other) {
    return false; // independent transformation
  }

  /**
   * Get the called fct name.
   * @return Fct name.
   */
  public String getCalledFctName(){
    return _calledFctName;
  }

  /**
   * Get the parent fct name.
   * @return Fct name.
   */
  public String getCallingFctName(){
    return _callingFctName;
  }
}
