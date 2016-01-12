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
    return isOfType(Xvar.class);
  }

  public Xvar getVar(){
    if(isVar()){
      return (Xvar)_element;
    }
    return null;
  }

  public boolean isIntConst(){
    return isOfType(XintConstant.class);
  }

  public boolean isRealConst(){
    return isOfType(XrealConstant.class);
  }

  public boolean isCharConst(){
    return isOfType(XcharacterConstant.class);
  }

  public boolean isLogicalConst(){
    return isOfType(XlogicalConstant.class);
  }

  public boolean isComplexConst(){
    return isOfType(XcomplexConstant.class);
  }

  public boolean isFctCall(){
    return isOfType(XfctCall.class);
  }

  public XintConstant getIntConstant(){
    if(isIntConst()){
      return (XintConstant)_element;
    }
    return null;
  }



  private boolean isOfType(Class type){
    if(_element != null && type.isInstance(_element)){
      return true;
    }
    return false;
  }


}
