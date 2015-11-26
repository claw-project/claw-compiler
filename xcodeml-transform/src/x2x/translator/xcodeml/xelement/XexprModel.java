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

public class XexprModel extends XbaseElement {

  public XexprModel(Element exprModelElement){
    super(exprModelElement);
  }
}
