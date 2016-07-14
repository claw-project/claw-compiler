/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */

package cx2x.xcodeml.xnode;

import org.w3c.dom.Element;

import java.util.ArrayList;
import java.util.List;

import cx2x.xcodeml.helper.*;

/**
 * The XbasicType represents the basicType (3.3) element in XcodeML intermediate
 * representation.
 *
 * Elements: (kind?, (len | (arrayIndex | indexRange)+)?, coShape?)
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
 * The type attribute is defined in the Xtype base class.
 *
 * @author clementval
 */

public class XbasicType extends Xtype {

  private boolean _isArray = false;

  // Optional elements
  private List<Xnode> _dimensions = null;
  private Xnode _kind = null;
  private Xnode _length = null;

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
  private Xintent _intent = null;

  public static final int APPEND = -1;

  /**
   * Element standard ctor. Pass the base element to the base class and read
   * inner information (elements and attributes).
   * @param baseElement The root of the element.
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

    _dimensions = XnodeUtil.findIndexes(this);
    // is array ?
    if (_dimensions.size() > 0){
      _isArray = true;
    }

    // has length ?
    _length = find(Xcode.LEN);

    // has kind ?
    _kind = find(Xcode.KIND);
  }

  /**
   * Read all required attributes.
   */
  private void readRequiredAttributes() {
    // Attribute type is read in Xtype
    _ref = getAttribute(Xattr.REF);
  }

  /**
   * Read all optional attributes
   */
  private void readOptionalAttributes() {
    _is_public = getBooleanAttribute(Xattr.IS_PUBLIC);
    _is_private = getBooleanAttribute(Xattr.IS_PRIVATE);
    _is_pointer = getBooleanAttribute(Xattr.IS_POINTER);
    _is_target = getBooleanAttribute(Xattr.IS_TARGET);
    _is_external = getBooleanAttribute(Xattr.IS_EXTERNAL);
    _is_intrinsic = getBooleanAttribute(Xattr.IS_INTRINSIC);
    _is_optional = getBooleanAttribute(Xattr.IS_OPTIONAL);
    _is_save = getBooleanAttribute(Xattr.IS_SAVE);
    _is_parameter = getBooleanAttribute(Xattr.IS_PARAMETER);
    _is_allocatable = getBooleanAttribute(Xattr.IS_ALLOCATABLE);
    _intent = Xintent.fromString(getAttribute(Xattr.INTENT));
  }

  /**
   * Get the indexRange object for the given dimension.
   * @param index The position of the dimension. For the first dimension, index
   *              is 0, for the second is 1 and so on.
   * @return A XindexRange object representing the index range of a specific
   * dimension.
   */
  public Xnode getDimensions(int index){
    if(index >= _dimensions.size() || index < 0){
      return null;
    }
    return _dimensions.get(index);
  }

  /**
   * Check whether the type is an array type.
   * @return True if the type is an array type. False otherwise.
   */
  public boolean isArray(){
    return _isArray;
  }

  /**
   * Check whether the type has a length element.
   * @return True if the type has a length element. False otherwise.
   */
  public boolean hasLength(){
    return _length != null;
  }

  /**
   * Get the len element.
   * @return Len element. Null if the basic type has no len element.
   */
  public Xnode getLength(){
    return _length;
  }

  /**
   * Check whether the type has a kind element.
   * @return True if the type has a kind element. False otherwise.
   */
  public boolean hasKind(){
    return _kind != null;
  }

  /**
   * Get the kind element.
   * @return Kind element. Null if the basic type has no kind element.
   */
  public Xnode getKind(){
    return _kind;
  }

  /**
   * Get the array dimensions.
   * @return The dimensions of the array type.
   */
  public int getDimensions(){
    return _dimensions.size();
  }

  /**
   * Get ref attribute value.
   * @return The ref attribute value as String.
   */
  public String getRef(){
    return _ref;
  }

  /**
   * Set the value of ref attribute.
   * @param value New value of ref attribute.
   */
  public void setRef(String value){
    setAttribute(Xattr.REF, value);
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

  /**
   * Check whether the type has an intent.
   * @return True if the type has an intent. False otherwise.
   */
  public boolean hasIntent(){
    return _intent != Xintent.NONE;
  }

  /**
   * Get the intent of the type.
   * @return Intent. Null if the type has no intent.
   */
  public Xintent getIntent(){
    return _intent;
  }

  /**
   * Set the intent of the type.
   * @param value Intent value to be set.
   */
  public void setIntent(Xintent value){
    setAttribute(Xattr.INTENT, value.toString());
    _intent = value;
  }

  /**
   * Remove intent attribute from the element.
   */
  public void removeIntent(){
    if(hasIntent()) {
      _baseElement.removeAttribute(Xname.ATTR_INTENT);
      _intent = null;
    }
  }

  /**
   * Remove is_allocatable attribute from the element.
   */
  public void removeAllocatable(){
    if(isAllocatable()){
      _baseElement.removeAttribute(Xname.ATTR_IS_ALLOCATABLE);
      _is_allocatable = false;
    }
  }

  /**
   * Remove all dimension from the type
   */
  public void resetDimension(){
    for(Xnode idx : _dimensions){
      idx.delete();
    }
    _dimensions.clear();
    _isArray = false;
  }

  /**
   * Remove the dimensions not in the given list. Dimension index starts at 1.
   * @param keptDimensions List of dimension index to be kept.
   */
  public void removeDimension(List<Integer> keptDimensions){
    List<Xnode> keptDim = new ArrayList<>();
    for(int i = 0; i < _dimensions.size(); i++){
      if(keptDimensions.contains(i+1)){
        keptDim.add(_dimensions.get(i));
      } else {
        _dimensions.get(i).delete();
      }
    }
    if(keptDim.size() == 0){
      _isArray = false;
    }
    _dimensions = keptDim;
  }

  /**
   * Add a dimension to the basic type.
   * @param index    Index element to add as the new dimension.
   * @param position Position compared to already existing element. If -1,
   *                 dimension is added at the end.
   */
  public void addDimension(Xnode index, int position){
    if(_dimensions.size() == 0 || position == APPEND){
      appendToChildren(index, false);
      _dimensions.add(index);
      _isArray = true;
    } else {
      Xnode crtPos = _dimensions.get(position);
      XnodeUtil.insertBefore(crtPos, index);
      _dimensions.add(position, index);
    }
  }

  @Override
  public XbasicType cloneObject() {
    Element element = (Element)cloneNode();
    return new XbasicType(element);
  }
}
