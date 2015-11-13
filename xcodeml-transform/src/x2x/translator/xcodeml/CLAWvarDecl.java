package x2x.translator.xcodeml;

import org.w3c.dom.Element;
import org.w3c.dom.Node;

/*
<varDecl lineno="5" file="original_code.f90">
  <name type="A7fd318c08290">value2</name>
</varDecl>
<varDecl lineno="6" file="original_code.f90">
  <name type="Fint">i</name>
</varDecl>
<varDecl lineno="7" file="original_code.f90">
  <name type="Fint">istart</name>
  <value>
    <FintConstant type="Fint">1</FintConstant>
  </value>
</varDecl>*/

public class CLAWvarDecl {
  private String _type = null;
  private String _name = null;
  private boolean _hasValue = false;

  private Element _varDeclElement;

  // TODO keep as element for the moment but it should be read as well
  private Element _valueElement;

  public CLAWvarDecl(Element varDeclElement){
    _varDeclElement = varDeclElement;
    readElementInformation();
  }

  public Node clone(){
    return _varDeclElement.cloneNode(true);
  }

  private void readElementInformation(){
    Element nameElement = CLAWelementHelper.findFirstElement(_varDeclElement,
      XelementName.NAME);
    _name = nameElement.getTextContent();
    _type = CLAWelementHelper.getAttributeValue(nameElement,
      XelementName.ATTR_TYPE);
    _valueElement = CLAWelementHelper.findFirstElement(_varDeclElement,
        XelementName.VALUE);
    if(_valueElement != null){
      _hasValue = true;
    }
  }

  public boolean hasValue(){
    return _hasValue;
  }

  public String getName(){
    return _name;
  }

}
