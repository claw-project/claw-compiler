/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */

package cx2x.xcodeml.xnode;

import org.w3c.dom.Element;

import java.util.List;

/**
 * The Xparams represents the params (8.5) element in XcodeML intermediate
 * representation.
 * <p>
 * Elements: (name*)
 * - Optional:
 * - name (Xname)
 *
 * @author clementval
 */
public class Xparams extends Xnode {

  private List<Xnode> _parameters = null;

  /**
   * Element standard ctor. Pass the base element to the base class and read
   * inner information (elements and attributes).
   *
   * @param baseElement The root of the element.
   */
  public Xparams(Element baseElement) {
    super(baseElement);
    _parameters = matchAll(Xcode.NAME);
  }

  /**
   * Get the number of parameters in the params element.
   *
   * @return Number of parameters.
   */
  public int count() {
    return _parameters.size();
  }

  /**
   * Get the list of all name elements.
   *
   * @return List of Xname objects.
   */
  public List<Xnode> getAll() {
    return _parameters;
  }

  /**
   * Add a name element to the parameters list.
   *
   * @param name The name element to add.
   */
  public void add(Xnode name) {
    _parameters.add(name);
    append(name);
  }

  /**
   * Add a name element to the parameters list before the referenced element.
   *
   * @param ref  Referenced element. New element will be added before.
   * @param name The name element to add.
   */
  public void addBefore(Xnode ref, Xnode name) {
    int index = _parameters.indexOf(ref);
    _parameters.add(index, name);
    ref.insertBefore(name);
  }

  @Override
  public Xparams cloneNode() {
    Element clone = (Element) cloneRawNode();
    return new Xparams(clone);
  }
}
