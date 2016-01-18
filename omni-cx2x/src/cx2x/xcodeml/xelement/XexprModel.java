/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */

package cx2x.xcodeml.xelement;


/**
 * The XexprModel represents the exprModel (9.4) element in XcodeML
 * intermediate representation.
 *
 * Elements:
 * - Required: on of the followings elements:
 *   - FintConstant (XintConstant), FrealConstant (XrealConstant),
 *     FcomplexConstant (XcomplexConstant), FcharacterConstant
 *     (XcharacterConstant), FlogicalConstant (XlogicalConstant)
 *   - FarrayConstructor TODO, FstructConstructor TODO
 *   - Var (Xvar)
 *   - FarrayRef (XarrayRef), FcharacterRef TODO, FmemberRef TODO,
 *     FcoArrayRef TODO, varRef (XvarRef)
 *   - functionCall (XfctCall)
 *   - plusExpr, minusExpr, mulExpr, divExpr, FpowerExpr, FconcatExpr
 *   - logEQExpr, logNEQExpr, logGEExpr, logGTExpr, logLEExpr, logLTExpr,
 *     logAndExpr, logOrExpr, logEQVExpr, logNEQVExpr, logNotExpr TODO ALL
 *   - unaryMinusExpr, userBinaryExpr, userUnaryExpr TODO
 *   - FdoLoop (Xdo)
 *
 * @author clementval
 */

public class XexprModel {
  private XbaseElement _element = null;

  /**
   * Constructs a new XexprModel object from an XbaseElement.
   * @param baseElement The root XbaseElement.
   */
  public XexprModel(XbaseElement baseElement){
    _element = baseElement;
  }

  /**
   * Get the root XbaseElement.
   * @return The root XbaseElement.
   */
  public XbaseElement getElement(){
    return _element;
  }

  /**
   * Set the root XbaseElement.
   * @param element The root XbaseElement.
   */
  public void setElement(XbaseElement element){
    _element = element;
  }

  /**
   * Check whether the exprModel is a var.
   * @return True if the exprModel is a var. False otherwise.
   */
  public boolean isVar(){
    return isOfType(Xvar.class);
  }

  /**
   * Get the exprModel as var.
   * @return Xvar object if the exprModel is a var. Null otherwise.
   */
  public Xvar getVar(){
    if(isVar()){
      return (Xvar)_element;
    }
    return null;
  }

  /**
   * Check whether the exprModel is an integer constant.
   * @return True if the exprModel is an integer constant. False otherwise.
   */
  public boolean isIntConst(){
    return isOfType(XintConstant.class);
  }

  /**
   * Check whether the exprModel is a real constant.
   * @return True if the exprModel is a real constant. False otherwise.
   */
  public boolean isRealConst(){
    return isOfType(XrealConstant.class);
  }

  /**
   * Check whether the exprModel is a character constant.
   * @return True if the exprModel is a character constant. False otherwise.
   */
  public boolean isCharConst(){
    return isOfType(XcharacterConstant.class);
  }

  /**
   * Check whether the exprModel is a logical constant.
   * @return True if the exprModel is a logical constant. False otherwise.
   */
  public boolean isLogicalConst(){
    return isOfType(XlogicalConstant.class);
  }

  /**
   * Check whether the exprModel is a complex constant.
   * @return True if the exprModel is a complex constant. False otherwise.
   */
  public boolean isComplexConst(){
    return isOfType(XcomplexConstant.class);
  }

  /**
   * Check whether the exprModel is a function call.
   * @return True if the exprModel is a function call. False otherwise.
   */
  public boolean isFctCall(){
    return isOfType(XfctCall.class);
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
   * Check whether the exprModel is of a given class type.
   * @param type Class to be checked (Derived type of XbaseElement).
   * @return True if the exprModel is of the given class. False otherwise.
   */
  private <T extends XbaseElement> boolean isOfType(Class<T> type){
    if(_element != null && type.isInstance(_element)){
      return true;
    }
    return false;
  }


}
