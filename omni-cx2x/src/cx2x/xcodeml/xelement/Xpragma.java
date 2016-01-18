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
    Node clone = clone();
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
}
