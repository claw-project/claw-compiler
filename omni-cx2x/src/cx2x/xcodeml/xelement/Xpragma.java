/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */

package cx2x.xcodeml.xelement;

import org.w3c.dom.Element;
import org.w3c.dom.Node;

/**
 * The Xpragma represents the FpragmaStatement (6.25) element in XcodeML
 * intermediate representation.
 *
 * Elements: contains value of the pragma line.
 *
 * @author clementval
 */

public class Xpragma extends XenhancedElement implements Xclonable<Xpragma> {

  /**
   * Xelement standard ctor. Pass the base element to the base class and read
   * inner information (elements and attributes).
   * @param baseElement The root element of the Xelement
   */
  public Xpragma(Element baseElement){
    super(baseElement);
  }

  /**
   * Clone the current object.
   * @return A new object Xpragma that is the clone of the current object.
   */
  public Xpragma cloneObject(){
    Node clone = cloneNode();
    return new Xpragma((Element)clone);
  }

  /**
   * Append information to the current pragma.
   * @param data New information to be inserted.
   */
  public void append(String data){
    setValue(getValue() + " " + data);
  }

}
