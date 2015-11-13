package x2x.translator.xcodeml;

import org.w3c.dom.Element;

public class CLAWfctDef extends CLAWfct {

  public CLAWfctDef(Element fctDefElement){
    super(fctDefElement);
  }

  public Element getBody(){
    return CLAWelementHelper.getBody(getFctElement());
  }
}
