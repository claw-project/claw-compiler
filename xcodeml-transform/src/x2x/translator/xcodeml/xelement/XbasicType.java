package x2x.translator.xcodeml.xelement;

import org.w3c.dom.Element;
import org.w3c.dom.Node;

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
public class XbasicType {
  private Element _element;
  private String _type;
  private String _ref;

  private int _dimension = 0;


  public XbasicType(Element element){
    _element = element;
    readElementInformation();
  }

  private void readElementInformation(){
    _type = XelementHelper.getAttributeValue(_element,
      XelementName.ATTR_TYPE);
    _ref = XelementHelper.getAttributeValue(_element,
      XelementName.ATTR_REF);

    // is array ?
    _dimension = XelementHelper.findNumberOfRange(_element);

    // has length ?

  }

  public int getDimension(){
    return _dimension;
  }

  public String getRef(){
    return _ref;
  }

  public String getType(){
    return _type;
  }

  public Node clone(){
    return _element.cloneNode(true);
  }

}
