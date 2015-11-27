package x2x.translator.xcodeml.xelement;

import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

/**
 * The XexprModel represents the exprModel (9.4) element in XcodeML
 * intermediate representation.
 *
 * Elements:
 * - Required: on of the followings elements:
 *   - FintConstant (XintConstant), FrealConstant (XrealConstant),
 *     FcomplexConstant TODO, FcharacterConstant (TODO), FlogicalConstant TODO
 *   - FarrayConstructor, FstructConstructor
 *   - Var (Xvar)
 *   - FarrayRef, FcharacterRef, FmemberRef, FcoArrayRef, varRef
 *   - functionCall
 *   - plusExpr, minusExpr, mulExpr, divExpr, FpowerExpr, FconcatExpr
 *   - logEQExpr, logNEQExpr, logGEExpr, logGTExpr, logLEExpr, logLTExpr,
 *     logAndExpr, logOrExpr, logEQVExpr, logNEQVExpr, logNotExpr
 *   - unaryMinusExpr, userBinaryExpr, userUnaryExpr
 *   - FdoLoop
 */

public class XexprModel {
  private XbaseElement _element = null;

  public XexprModel(XbaseElement element){
    _element = element;
  }

  public XbaseElement getElement(){
    return _element;
  }

  public void setElement(XbaseElement element){
    _element = element;
  }

  public boolean isVar(){
    if(_element != null && _element instanceof Xvar){
      return true;
    }
    return false;
  }

  public Xvar getVar(){
    if(isVar()){
      return (Xvar)_element;
    }
    return null;
  }

  public boolean isIntConst(){
    if(_element != null && _element instanceof XintConstant){
      return true;
    }
    return false;
  }

  public XintConstant getIntConstant(){
    if(isIntConst()){
      return (XintConstant)_element;
    }
    return null;
  }


}
