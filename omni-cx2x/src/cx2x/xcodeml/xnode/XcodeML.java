/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */

package cx2x.xcodeml.xnode;

import org.w3c.dom.Document;

/**
 * The XcodeML class represents the basic XcodeML file unit. Both XcodeProgram
 * and Xmod inherit from this class.
 *
 * @author clementval
 */
public class XcodeML extends Xnode {

  /**
   * Constructs a basic XcodeML object representing the XcodeML file given in
   * input.
   * @param baseElement Document representing the XcodeML file.
   */
  public XcodeML(Document baseElement){
    super(baseElement.getDocumentElement());
  }
}
