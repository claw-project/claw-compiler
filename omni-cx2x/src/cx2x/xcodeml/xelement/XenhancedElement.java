/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */

package cx2x.xcodeml.xelement;

import cx2x.xcodeml.helper.XelementHelper;
import org.w3c.dom.Element;

/**
 * An enhanced element has two attributes that allow to know it's position in
 * the source file and its source file name.
 *
 * Attributes:
 * - Required:
 *   - lineno (text)
 *   - file (text)
 *
 * @author clementval
 */
public class XenhancedElement extends XbaseElement {

  // Attributes
  private int _lineno = 0;
  private String _file = null;

  /**
   * Xelement standard ctor. Pass the base element to the base class and read
   * inner information (elements and attributes).
   *
   * @param baseElement The root element of the Xelement
   */
  public XenhancedElement(Element baseElement) {
    super(baseElement);
    String lineno = XelementHelper.getAttributeValue(this,
        XelementName.ATTR_LINENO);
    try {
      _lineno = Integer.parseInt(lineno);
    } catch (NumberFormatException ex) {
      _lineno = 0;
    }
    _file = XelementHelper.getAttributeValue(this, XelementName.ATTR_FILE);
  }


  /**
   * Get the source file name.
   *
   * @return Source file name.
   */
  public String getFile() {
    return _file;
  }

  /**
   * Get the start line number of the element
   *
   * @return Line number.
   */
  public int getLineNo() {
    return _lineno;
  }


  /**
   * Set the line number of the pragma statement.
   *
   * @param lineno Line number.
   */
  public void setLine(int lineno) {
    if (baseElement != null) {
      baseElement.setAttribute(XelementName.ATTR_LINENO, "" + lineno);
      _lineno = lineno;
    }
  }

  /**
   * Set the filename attribute.
   *
   * @param filename Filename.
   */
  public void setFile(String filename) {
    if (baseElement != null) {
      baseElement.setAttribute(XelementName.ATTR_FILE, filename);
      _file = filename;
    }
  }
}
