/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */

package cx2x.xcodeml.xelement;

import org.w3c.dom.Element;
import org.w3c.dom.Node;
import cx2x.xcodeml.helper.*;

/**
 * The Xpragma represents the FpragmaStatement (6.25) element in XcodeML
 * intermediate representation.
 *
 * Elements: contains value of the pragma line.
 *
 * @author clementval
 */

public class Xpragma extends XbaseElement implements Xclonable<Xpragma> {
  private String _value = null;
  private int _line = 0;
  private String _filename = null;

  /**
   * Xelement standard ctor. Pass the base element to the base class and read
   * inner information (elements and attributes).
   * @param baseElement The root element of the Xelement
   */
  public Xpragma(Element baseElement){
    super(baseElement);
    if(baseElement != null){
      _value = getData();
      String lineAttr = XelementHelper.getAttributeValue(this,
        XelementName.ATTR_LINENO);
      if(lineAttr != null){
        _line = Integer.parseInt(lineAttr);
      }
      _filename = XelementHelper.getAttributeValue(this,
        XelementName.ATTR_FILE);
    }
  }

  /**
   * Get the filename attribute value.
   * @return Filename value.
   */
  public String getFilename(){
    return _filename;
  }

  /**
   * Get the line attribute value.
   * @return Line value.
   */
  public int getLine(){
    return _line;
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
   * Set pragma data.
   * @param value New pragma data.
   */
  public void setData(String value){
    if(baseElement != null){
      baseElement.setTextContent(value);
      _value = value;
    }
  }

  /**
   * Set the line number of the pragma statement.
   * @param lineno Line number.
   */
  public void setLine(int lineno){
    if(baseElement != null){
      baseElement.setAttribute(XelementName.ATTR_LINENO, "" + lineno);
      _line = lineno;
    }
  }

  /**
   * Set the filename attribute.
   * @param filename Filename.
   */
  public void setFilename(String filename){
    if(baseElement != null){
      baseElement.setAttribute(XelementName.ATTR_FILE, filename);
      _filename = filename;
    }
  }

  /**
   * Create an empty pragma element in the given program
   * @return A new pragma element with no children.
   */
  public static Xpragma createEmpty(XcodeProgram xcodeml){
    Element element = xcodeml.getDocument().
        createElement(XelementName.PRAGMA_STMT);
    return new Xpragma(element);
  }
}
