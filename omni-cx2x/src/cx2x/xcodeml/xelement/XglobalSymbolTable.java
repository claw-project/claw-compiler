/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */

package cx2x.xcodeml.xelement;

import org.w3c.dom.Element;

/**
 * The XglobalSymbolTable represents the globalSymbols (4.1) element in
 * XcodeML intermediate representation.
 *
 * Implementation is done in XsymbolTable as they are identical. Just the
 * root element name differs.
 *
 * Elements:
 * - Optional:
 *   - id
 *
 * @author clementval
 */
 
public class XglobalSymbolTable extends XsymbolTable {

  public XglobalSymbolTable(Element baseElement){
    super(baseElement);
  }

}
