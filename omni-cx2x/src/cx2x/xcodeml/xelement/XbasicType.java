/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */

package cx2x.xcodeml.xelement;

import org.w3c.dom.Element;
import java.util.ArrayList;
import cx2x.xcodeml.helper.*;

/**
 * The XbasicType represents the basicType (3.3) element in XcodeML intermediate
 * representation.
 *
 * Elements:
 * - Optional:
 *   - kind (Xkind)
 *   - len (Xlength)
 *   - arrayIndex (XarrayIndex)
 *   - indexRange (XindexRange)
 *   - coShape TODO not needed for the moment
 * Attributes:
 * - Required: type (text), ref (text)
 * - Optional: is_public (bool), is_private (bool), is_pointer (bool),
 *             is_target (bool), is_external (bool),is_intrinsic (bool),
 *             is_optional (bool), is_save (bool), is_parameter (bool),
 *             is_allocatable (bool), intent (text: in, out, inout)
 *
 * @author clementval
 */

public class XbasicType extends Xtype {


  private int _dimension = 0;
  private boolean _isArray = false;

  // Optional elements
  private ArrayList<XindexRange> _dimensionRanges = null;
  private XarrayIndex _arrayIndex = null; // TODO can be 1 to N
  private Xkind _kind = null;
  private Xlength _length = null;

  // XbasicType required attributes (type is declared in Xtype)
  private String _ref;

  // XbasicType optional attributes
  private boolean _is_public = false;
  private boolean _is_private = false;
  private boolean _is_pointer = false;
  private boolean _is_target = false;
  private boolean _is_external = false;
  private boolean _is_intrinsic = false;
  private boolean _is_optional = false;
  private boolean _is_save = false;
  private boolean _is_parameter = false;
  private boolean _is_allocatable = false;
  // TODO intent as enum

  /**
   * Xelement standard ctor. Pass the base element to the base class and read
   * inner information (elements and attributes).
   * @param baseElement The root element of the Xelement
   */
  public XbasicType(Element baseElement){
    super(baseElement);
    readBasicTypeInformation();
  }

  /**
   * Read inner element information.
   */
  private void readBasicTypeInformation(){
    readRequiredAttributes();
    readOptionalAttributes();

    // is array ?
    _dimension = XelementHelper.findNumberOfRange(this);
    if (_dimension > 0){
      _isArray = true;
      _dimensionRanges = XelementHelper.findIndexRanges(this);
    }

    // has length ?
    _length = XelementHelper.findLen(this, false);

    // has kind ?
    _kind = XelementHelper.findKind(this, false);

    // has arrayIndex ?
    _arrayIndex = XelementHelper.findArrayIndex(this, false);
  }

  /**
   * Read all required attributes.
   */
  private void readRequiredAttributes() {
    // Attribute type is read in Xtype
    _ref = XelementHelper.getAttributeValue(this, XelementName.ATTR_REF);
  }

  /**
   * Read all optional attributes
   */
  private void readOptionalAttributes() {
    _is_public = XelementHelper.getBooleanAttributeValue(this,
      XelementName.ATTR_IS_PUBLIC);
    _is_private = XelementHelper.getBooleanAttributeValue(this,
      XelementName.ATTR_IS_PRIVATE);
    _is_pointer = XelementHelper.getBooleanAttributeValue(this,
      XelementName.ATTR_IS_POINTER);
    _is_target = XelementHelper.getBooleanAttributeValue(this,
      XelementName.ATTR_IS_TARGET);
    _is_external = XelementHelper.getBooleanAttributeValue(this,
      XelementName.ATTR_IS_EXTERNAL);
    _is_intrinsic = XelementHelper.getBooleanAttributeValue(this,
      XelementName.ATTR_IS_INTRINSIC);
    _is_optional = XelementHelper.getBooleanAttributeValue(this,
      XelementName.ATTR_IS_OPTIONAL);
    _is_save = XelementHelper.getBooleanAttributeValue(this,
      XelementName.ATTR_IS_SAVE);
    _is_parameter = XelementHelper.getBooleanAttributeValue(this,
      XelementName.ATTR_IS_PARAMETER);
    _is_allocatable = XelementHelper.getBooleanAttributeValue(this,
      XelementName.ATTR_IS_ALLOCATABLE);
  }

  /**
   * Get the indexRange object for the given dimension.
   * @param index The position of the dimension. For the first dimension, index
   *              is 0, for the second is 1 and so on.
   * @return A XindexRange object representing the index range of a specific
   * dimension.
   */
  public XindexRange getDimensions(int index){
    if(index >= _dimensionRanges.size() || index < 0){
      return null;
    }
    return _dimensionRanges.get(index);
  }

  /**
   * Check whether the type is an array type.
   * @return True if the type is an array type. False otherwise.
   */
  public boolean isArray(){
    return _isArray;
  }

  /**
   * Check whether the type has a length attribute.
   * @return True if the type has a length attribute. False otherwise.
   */
  public boolean hasLength(){
    return _length != null;
  }

  /**
   * Check whether the type has a kind attribute.
   * @return True if the type has a kind attribute. False otherwise.
   */
  public boolean hasKind(){
    return _kind != null;
  }

  /**
   * Check whether the type has array indexes.
   * @return True if the type has array indexes. False otherwise.
   */
  public boolean hasArrayIndex(){
    return _arrayIndex != null;
  }

  /**
   * Get the array dimensions.
   * @return The dimensions of the array type.
   */
  public int getDimensions(){
    return _dimension;
  }

  /**
   * Get ref attribute value.
   * @return The ref attribute value as String.
   */
  public String getRef(){
    return _ref;
  }

  /**
   * Check whether the type is public.
   * @return True if the type is public. False otherwise.
   */
  public boolean isPublic() {
    return _is_public;
  }

  /**
   * Check whether the type is private.
   * @return True if the type is private. False otherwise.
   */
  public boolean isPrivate() {
    return _is_private;
  }

  /**
   * Check whether the type is a pointer.
   * @return True if the type is a pointer. False otherwise.
   */
  public boolean isPointer() {
    return _is_pointer;
  }

  /**
   * Check whether the type is a target.
   * @return True if the type is a target. False otherwise.
   */
  public boolean isTarget() {
    return _is_target;
  }

  /**
   * Check whether the type is external.
   * @return True if the type is external. False otherwise.
   */
  public boolean isExternal() {
    return _is_external;
  }

  /**
   * Check whether the type is intrinsic.
   * @return True if the type is intrinsic. False otherwise.
   */
  public boolean isIntrinsic() {
    return _is_intrinsic;
  }

  /**
   * Check whether the type is optional.
   * @return True if the type is optional. False otherwise.
   */
  public boolean isOptional(){
    return _is_optional;
  }

  /**
   * Check whether the type is save.
   * @return True if the type is save. False otherwise.
   */
  public boolean isSave() {
    return _is_save;
  }

  /**
   * Check whether the type is a parameter.
   * @return True if the type is a parameter. False otherwise.
   */
  public boolean isParameter() {
    return _is_parameter;
  }

  /**
   * Check whether the type is allocatable.
   * @return True if the type is allocatable. False otherwise.
   */
  public boolean isAllocatable() {
    return _is_allocatable;
  }

}
