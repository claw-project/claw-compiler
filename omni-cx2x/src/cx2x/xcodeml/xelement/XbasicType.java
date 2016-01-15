/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */

package cx2x.xcodeml.xelement;

import org.w3c.dom.Element;

import java.util.ArrayList;

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

  public XbasicType(Element element){
    super(element);
    readBasicTypeInformation();
  }

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

  private void readRequiredAttributes() {
    // Attribute type is read in Xtype
    _ref = XelementHelper.getAttributeValue(this, XelementName.ATTR_REF);
  }

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

  public XindexRange getDimensions(int index){
    if(index >= _dimensionRanges.size() || index < 0){
      return null;
    }
    return _dimensionRanges.get(index);
  }

  public boolean isArray(){
    return _isArray;
  }

  public boolean hasLength(){
    return _length != null;
  }

  public boolean hasKind(){
    return _kind != null;
  }

  public boolean hasArrayIndex(){
    return _arrayIndex != null;
  }

  public int getDimensions(){
    return _dimension;
  }

  public String getRef(){
    return _ref;
  }

  public boolean isPublic() {
    return _is_public;
  }

  public boolean isPrivate() {
    return _is_private;
  }

  public boolean isPointer() {
    return _is_pointer;
  }

  public boolean isTarget() {
    return _is_target;
  }

  public boolean isExternal() {
    return _is_external;
  }

  public boolean isIntrinsic() {
    return _is_intrinsic;
  }

  public boolean isOptional(){
    return _is_optional;
  }

  public boolean isSave() {
    return _is_save;
  }

  public boolean isParameter() {
    return _is_parameter;
  }

  public boolean isAllocatable() {
    return _is_allocatable;
  }

}
