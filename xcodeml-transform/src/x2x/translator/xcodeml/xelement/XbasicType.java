package x2x.translator.xcodeml.xelement;

import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;
/*
<FbasicType type="A7f9c1a5046e0" ref="Fint">
  <indexRange>
    <lowerBound>
      <FintConstant type="Fint">1</FintConstant>
    </lowerBound>
    <upperBound>
      <FintConstant type="Fint">10</FintConstant>
    </upperBound>
  </indexRange>
</FbasicType>

<FbasicType type="C7f9c1a50ae40" ref="Fcharacter">
  <len>
    <FintConstant type="Fint">8</FintConstant>
  </len>
</FbasicType>
*/
public class XbasicType extends Xtype {

  private String _ref;
  private int _dimension = 0;
  private int _length = 0;
  private boolean _isArray = false;
  private boolean _hasLength = false;
  private XindexRange[] _dimensionRanges = null;

  public XbasicType(Element element){
    super(element);
    readBasicTypeInformation();
  }

  private void readBasicTypeInformation(){
    _ref = XelementHelper.getAttributeValue(_element,
      XelementName.ATTR_REF);

    // is array ?
    _dimension = XelementHelper.findNumberOfRange(_element);
    if (_dimension > 0){
      _isArray = true;
      _dimensionRanges = new XindexRange[_dimension];
      NodeList ranges = XelementHelper.findIndexRanges(_element);
      for(int i = 0; i < _dimension; ++i){
        _dimensionRanges[i] = new XindexRange((Element)ranges.item(i));
      }
    }

    // has length ?
    Element length = XelementHelper.findLen(_element);
    if(length != null){
      _hasLength = true;
      // TODO have a length object with information
    }
  }

  public XindexRange getDimensions(int index){
    if(index >= _dimensionRanges.length || index < 0){
      return null;
    }
    return _dimensionRanges[index];
  }

  public boolean isArray(){
    return _isArray;
  }

  public boolean hasLength(){
    return _hasLength;
  }

  public int getDimensions(){
    return _dimension;
  }

  public String getRef(){
    return _ref;
  }

}
