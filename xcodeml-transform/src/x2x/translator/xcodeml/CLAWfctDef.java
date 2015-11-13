package x2x.translator.xcodeml;

import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;
import java.util.Hashtable;

/*
<symbols>
  <id type="A7fd318c079e0" sclass="flocal">
    <name>value1</name>
  </id>
  <id type="A7fd318c08290" sclass="flocal">
    <name>value2</name>
  </id>
  <id type="Fint" sclass="flocal">
    <name>i</name>
  </id>
  <id type="Fint" sclass="flocal">
    <name>istart</name>
  </id>
  <id type="Fint" sclass="flocal">
    <name>iend</name>
  </id>
  <id type="F7fd318c0a260" sclass="ffunc">
    <name>clawloop</name>
  </id>
</symbols>
<declarations>
  <varDecl lineno="4" file="original_code.f90">
    <name type="A7fd318c079e0">value1</name>
  </varDecl>
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
  </varDecl>
  <varDecl lineno="8" file="original_code.f90">
    <name type="Fint">iend</name>
    <value>
      <FintConstant type="Fint">10</FintConstant>
    </value>
  </varDecl>
</declarations>
*/

public class CLAWfctDef extends CLAWfct {

  private Hashtable<String, CLAWid> _symbolTable;


  public CLAWfctDef(Element fctDefElement){
    super(fctDefElement);

    _symbolTable = new Hashtable<String, CLAWid>();
    readSymbolsTable();
  }

  public Hashtable<String, CLAWid> getSymbolTable(){
    return _symbolTable;
  }

  private void readSymbolsTable(){
    Element symbols = CLAWelementHelper.findSymbols(getFctElement());
    NodeList nodeList = getFctElement().getElementsByTagName(XelementName.ID);
    for (int i = 0; i < nodeList.getLength(); i++) {
      Node idNode = nodeList.item(i);
      if (idNode.getNodeType() == Node.ELEMENT_NODE) {
        Element idElement = (Element) idNode;
        CLAWid id = new CLAWid(idElement);
        _symbolTable.put(id.getName(), id);
      }
    }
  }

  public Element getBody(){
    return CLAWelementHelper.getBody(getFctElement());
  }
}
