/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */

package cx2x.xcodeml.xelement;

import org.w3c.dom.Element;

/**
 * The XglobalDeclTable represents the typeTable (5.1) element in XcodeML
 * intermediate representation.
 *
 * Elements:
 * - Optional:
 *   - varDecl
 *   - FstructDecl
 *   - externDecl
 *   - FuseDecl
 *   - FuseOnlyDecl
 *   - FinterfaceDecl
 *   - FnamelistDecl
 *   - FequivalenceDecl
 *   - FcommonDecl
 *
 * All implementation is done in the base class XdeclTable.
 *
 * @author clementval
 */
public class XglobalDeclTable extends XdeclTable {

  /**
   * Xelement standard ctor. Pass the base element to the base class and read
   * inner information (elements and attributes).
   * @param baseElement The root element of the Xelement
   */
  public XglobalDeclTable(Element baseElement){
    super(baseElement);
  }

}
