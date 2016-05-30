/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */

package cx2x.xcodeml.xelement;

import cx2x.xcodeml.exception.IllegalTransformationException;
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

public class XbasicType extends Xtype implements Xclonable<XbasicType> {


  private boolean _isArray = false;

  // Optional elements
  private List<Xindex> _dimensions = null;
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
  private Xintent _intent = null;

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


    _dimensions = XelementHelper.findIndexes(this);
    // is array ?
    if (_dimensions.size() > 0){
      _isArray = true;
    }

    // has length ?
    _length = XelementHelper.findLen(this, false);

    // has kind ?
    _kind = XelementHelper.findKind(this, false);
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

    String intentValue = XelementHelper.getAttributeValue(this,
        XelementName.ATTR_INTENT);


    _intent = Xintent.fromString(intentValue);



  }

  /**
   * Get the indexRange object for the given dimension.
   * @param index The position of the dimension. For the first dimension, index
   *              is 0, for the second is 1 and so on.
   * @return A XindexRange object representing the index range of a specific
   * dimension.
   */
  public Xindex getDimensions(int index){
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
  public Xlength getLength(){
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
  public Xkind getKind(){
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
    if(baseElement != null){
      baseElement.setAttribute(XelementName.ATTR_REF, value);
    }
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
    if(value != null) {
      baseElement.setAttribute(XelementName.ATTR_INTENT, value.toString());
    }
    _intent = value;
  }

  /**
   * Remove intent attribute from the element.
   */
  public void removeIntent(){
    if(hasIntent()) {
      baseElement.removeAttribute(XelementName.ATTR_INTENT);
      _intent = null;
    }
  }

  /**
   * Remove is_allocatable attribute from the element.
   */
  public void removeAllocatable(){
    if(isAllocatable()){
      baseElement.removeAttribute(XelementName.ATTR_IS_ALLOCATABLE);
      _is_allocatable = false;
    }
  }

  /**
   * Remove all dimension from the type
   */
  public void resetDimension(){
    for(Xindex idx : _dimensions){
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
    List<Xindex> keptDim = new ArrayList<>();
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
   * @param position Position compared to already existing element.
   */
  public void addDimension(Xindex index, int position){
    if(_dimensions.size() == 0){
      this.appendToChildren(index, false);
      _isArray = true;
    } else {
      if(position == _dimensions.size() - 1){ // Add at the end
        Xindex last = _dimensions.get(_dimensions.size()-1);
        XelementHelper.insertAfter(last, index);
        _dimensions.add(index);
      } else {
        Xindex crtPos = _dimensions.get(position);
        XelementHelper.insertBefore(crtPos, index);
        _dimensions.add(position, index);
      }
    }
  }

  @Override
  public XbasicType cloneObject() {
    Element element = (Element)cloneNode();
    return new XbasicType(element);
  }


  /**
   * Constructs a new basicType element with the given information.
   * @param type    Type hash.
   * @param ref     Reference type.
   * @param intent  Optional intent information.
   * @param xcodeml Current XcodeML program unit.
   * @return A new XbasicType object with the new element inside.
   * @throws IllegalTransformationException If the element cannot be created.
   */
  public static XbasicType create(String type, String ref, Xintent intent,
                                  XcodeProgram xcodeml)
      throws IllegalTransformationException
  {
    XbasicType bt = XelementHelper.createEmpty(XbasicType.class, xcodeml);
    bt.setType(type);
    if(ref != null) {
      bt.setRef(ref);
    }
    if(intent != null) {
      bt.setIntent(intent);
    }
    bt.readBasicTypeInformation();
    return bt;
  }
}
