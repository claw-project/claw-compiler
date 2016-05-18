/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */

package cx2x.xcodeml.xelement;

import cx2x.xcodeml.helper.XelementHelper;
import org.w3c.dom.Element;

import java.util.List;

/**
 * The Xparams represents the params (8.5) element in XcodeMLintermediate
 * representation.
 *
 * Elements: (name*)
 * - Optional:
 *   - name (Xname)
 *
 * @author clementval
 */
public class Xparams extends XbaseElement {

  private List<Xname> _parameters = null;

  /**
   * Xelement standard ctor. Pass the base element to the base class and read
   * inner information (elements and attributes).
   * @param baseElement The root element of the Xelement
   */
  public Xparams(Element baseElement){
    super(baseElement);
    _parameters = XelementHelper.findAllNames(this);
  }

  /**
   * Get the number of parameters in the params element.
   * @return Number of parameters.
   */
  public int count(){
    return _parameters.size();
  }

  /**
   * Get the list of all name elements.
   * @return List of Xname objects.
   */
  public List<Xname> getAll(){
    return _parameters;
  }

  /**
   * Add a name element to the parameters list.
   * @param name The name element to add. 
   */
  public void add(Xname name){
    _parameters.add(name);
    appendToChildren(name, false);
  }

}
