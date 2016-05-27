/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */

package cx2x.xcodeml.xelement;

import org.w3c.dom.Element;

import java.util.ArrayList;
import java.util.List;

/**
 * The XcontainsStatement represents the FcontainsStatement (6.26) element in
 * XcodeML intermediate representation.
 *
 * Elements: ( FfunctionDefinition+ )
 * - Required:
 *   - FfunctionDefinition (XfunctionDefinition)
 *
 * @author clementval
 */
public class XcontainsStatement extends XenhancedElement {

  private final List<XfunctionDefinition> _functionDefinitions;

  public XcontainsStatement(Element baseElement){
    super(baseElement);
    _functionDefinitions = new ArrayList<>();
    readElementInformation();
  }

  /**
   * Read the inner element information.
   */
  private void readElementInformation(){
    // TODO
  }

}
