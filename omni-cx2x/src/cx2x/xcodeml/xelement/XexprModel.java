/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */

package cx2x.xcodeml.xelement;


import cx2x.xcodeml.helper.XelementHelper;

/**
 * The XexprModel represents the exprModel (9.4) element in XcodeML
 * intermediate representation.
 *
 * Elements:
 *
 * ( FintConstant | FrealConstant | FcomplexConstant | FcharacterConstant
 *   | FlogicalConstant | FarrayConstructor | FstructConstructor | Var
 *   | FarrayRef | FcharacterRef | FmemberRef | FcoArrayRef | varRef
 *   | functionCall | plusExpr | minusExpr | mulExpr | divExpr | FpowerExpr
 *   | FconcatExpr | logEQExpr | logNEQExpr | logGEExpr | logGTExpr | logLEExpr
 *   | logLTExpr | logAndExpr | logOrExpr | logEQVExpr | logNEQVExpr
 *   | unaryMinusExpr | logNotExpr | userBinaryExpr | userUnaryExpr | FdoLoop )
 *
 *
 * - Required: one of the following elements:
 *   - FintConstant (XintConstant), FrealConstant (XrealConstant),
 *     FcomplexConstant (XcomplexConstant), FcharacterConstant
 *     (XcharacterConstant), FlogicalConstant (XlogicalConstant)
 *   - FarrayConstructor TODO, FstructConstructor TODO
 *   - Var (Xvar)
 *   - FarrayRef (XarrayRef), FcharacterRef TODO, FmemberRef TODO,
 *     FcoArrayRef TODO, varRef (XvarRef)
 *   - functionCall (XfunctionCall)
 *   - plusExpr, minusExpr, mulExpr, divExpr, FpowerExpr, FconcatExpr
 *   - logEQExpr, logNEQExpr, logGEExpr, logGTExpr, logLEExpr, logLTExpr,
 *     logAndExpr, logOrExpr, logEQVExpr, logNEQVExpr, logNotExpr TODO ALL
 *   - unaryMinusExpr, userBinaryExpr, userUnaryExpr TODO
 *   - FdoLoop (Xdo)
 *
 * @author clementval
 */

public class XexprModel extends XbaseModel {


  /**
   * Constructs a new XexprModel object from an XbaseElement.
   * @param baseElement The root XbaseElement.
   */
  public XexprModel(XbaseElement baseElement){
    super(baseElement);
  }

  /**
   * Check whether the exprModel is an integer constant.
   * @return True if the exprModel is an integer constant. False otherwise.
   */
  public boolean isIntConst(){
    return isOfType(XintConstant.class);
  }

  /**
   * Get the exprModel as integer constant.
   * @return XintConstant object if the exprModel is an integer constant.
   * Null otherwise.
   */
  public XintConstant getIntConstant(){
    if(isIntConst()){
      return (XintConstant)_element;
    }
    return null;
  }

  /**
   * Check whether the exprModel is a real constant.
   * @return True if the exprModel is a real constant. False otherwise.
   */
  public boolean isRealConst(){
    return isOfType(XrealConstant.class);
  }

  /**
   * Get the exprModel as character constant.
   * @return XrealConstant object if the exprModel is a real constant.
   * Null otherwise.
   */
  public XrealConstant getRealConstant(){
    if(isRealConst()){
      return (XrealConstant)_element;
    }
    return null;
  }

  /**
   * Check whether the exprModel is a character constant.
   * @return True if the exprModel is a character constant. False otherwise.
   */
  public boolean isCharConst(){
    return isOfType(XcharacterConstant.class);
  }

  /**
   * Get the exprModel as character constant.
   * @return XcharacterConstant object if the exprModel is a character constant.
   * Null otherwise.
   */
  public XcharacterConstant getCharacterConstant(){
    if(isCharConst()){
      return (XcharacterConstant)_element;
    }
    return null;
  }

  /**
   * Check whether the exprModel is a logical constant.
   * @return True if the exprModel is a logical constant. False otherwise.
   */
  public boolean isLogicalConst(){
    return isOfType(XlogicalConstant.class);
  }

  /**
   * Get the exprModel as logical constant.
   * @return XlogicalConstant object if the exprModel is a logical constant.
   * Null otherwise.
   */
  public XlogicalConstant getLogicalConstant(){
    if(isLogicalConst()){
      return (XlogicalConstant)_element;
    }
    return null;
  }

  /**
   * Check whether the exprModel is a complex constant.
   * @return True if the exprModel is a complex constant. False otherwise.
   */
  public boolean isComplexConst(){
    return isOfType(XcomplexConstant.class);
  }

  /**
   * Check whether the exprModel is a constant type.
   * @return True if the exprModel is a constant type. False otherwise.
   */
  public boolean isConstant() {
    return isIntConst() || isRealConst() || isLogicalConst() || isCharConst()
        || isComplexConst();
  }

  /**
   * Get the constant element.
   * @return the constant element.
   */
  public Xconstant getConstant(){
    if(isConstant()){
      return (Xconstant)_element;
    }
    return null;
  }

  /**
   * Check whether the exprModel is a function call.
   * @return True if the exprModel is a function call. False otherwise.
   */
  public boolean isFctCall(){
    return isOfType(XfunctionCall.class);
  }

  /**
   * Get the exprModel as function call.
   * @return XfunctionCall object if the exprModel is a function call.
   * Null otherwise.
   */
  public XfunctionCall getFctCall(){
    if(isFctCall()){
      return (XfunctionCall) _element;
    }
    return null;
  }

}
