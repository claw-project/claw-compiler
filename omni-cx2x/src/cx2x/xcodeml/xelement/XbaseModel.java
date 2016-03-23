/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */

package cx2x.xcodeml.xelement;

/**
 * XbaseModel is the common part between the XexprModel and the XLValueModel
 * classes.
 *
 * Possible elements:
 * - Var (Xvar)
 * - FarrayRef (XarrayRef)
 * - FcharacterRef TODO
 * - FmemberRef TODO
 * - FcoArrayRef TODO
 *
 * (Var | FarrayRef | FcharacterRef | FmemberRef | FcoArrayRef)
 *
 * @author clementval
 */
public class XbaseModel {
  XbaseElement _element = null;

  /**
   * Constructs a new XbaseModel object from an XbaseElement.
   * @param baseElement The root XbaseElement.
   */
  XbaseModel(XbaseElement baseElement){
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
   * Check whether the exprModel is an arrayRef.
   * @return True if the exprModel is an arrayRef. False otherwise.
   */
  public boolean isArrayRef(){
    return isOfType(XarrayRef.class);
  }

  /**
   * Get the exprModel as var.
   * @return Xvar object if the exprModel is a var. Null otherwise.
   */
  public XarrayRef getArrayRef(){
    if(isArrayRef()){
      return (XarrayRef)_element;
    }
    return null;
  }

  /**
   * Check whether the exprModel is of a given class type.
   * @param type Class to be checked (Derived type of XbaseElement).
   * @return True if the exprModel is of the given class. False otherwise.
   */
  <T extends XbaseElement> boolean isOfType(Class<T> type) {
    return _element != null && type.isInstance(_element);
  }
}
