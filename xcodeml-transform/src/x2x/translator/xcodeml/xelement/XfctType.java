package x2x.translator.xcodeml.xelement;

import org.w3c.dom.Element;
import org.w3c.dom.Node;

/*
<FfunctionType type="F7fe5cad05ac0" return_type="Fvoid">
  <params>
    <name type="A7fe5cad072a0">value1</name>
    <name type="A7fe5cad07a50">value2</name>
  </params>
</FfunctionType>
*/
public class XfctType extends Xtype {

  private String _returnType = null;
  private boolean _isProgram = false;

  public XfctType(Element element){
    super(element);
    readFctTypeInformation();
  }

  private void readFctTypeInformation(){
    _returnType = XelementHelper.getAttributeValue(_element,
      XelementName.ATTR_RETURN_TYPE);

    String value = XelementHelper.getAttributeValue(_element,
      XelementName.ATTR_IS_PROGRAM);

    if(value != null && value.equals(XelementName.TRUE)){
      _isProgram = true;
    }

    // TODO read parameters as Xname element
  }

  public String getReturnType(){
    return _returnType;
  }

  public boolean isProgram(){
    return _isProgram;
  }

}
