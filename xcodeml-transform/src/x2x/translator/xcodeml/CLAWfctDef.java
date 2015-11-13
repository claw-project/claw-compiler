package x2x.translator.xcodeml;

import org.w3c.dom.Element;

public class CLAWfctDef extends CLAWfct {

  public CLAWfctDef(Element fctDefElement){
    super(fctDefElement);
  }

  public boolean hasLoop(){
    Element body = CLAWelementHelper.getBody(getFctElement());
    Element loop = CLAWelementHelper.findLoopStament(body);
    if(loop == null){
      return false;
    }
    return true;
  }
}
