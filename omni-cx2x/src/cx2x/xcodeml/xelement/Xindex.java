/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */

package cx2x.xcodeml.xelement;

import org.w3c.dom.Element;

/**
 * Xindex class is the common class for XarrayIndex (8.10) and
 * XindexRange (8.11).
 *
 * @see XarrayIndex
 * @see XindexRange
 *
 * @author clementval
 */
public class Xindex extends XbaseElement {

  /**
   * Xelement standard ctor. Pass the base element to the base class and read
   * inner information (elements and attributes).
   * @param baseElement The root element of the Xelement
   */
  public Xindex(Element baseElement){
    super(baseElement);
  }
}
