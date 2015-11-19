package x2x.translator.xcodeml.xelement;

import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;
import java.util.Hashtable;

/*
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

public class XfctDef extends Xfct {

  private XsymbolTable _symbolTable;
  private Hashtable<String, XvarDecl> _declTable;

  //private Element _symbolsElement = null;
  private Element _declarationsElement = null;

  public XfctDef(Element fctDefElement){
    super(fctDefElement);
    Element symbols = XelementHelper.findSymbols(getFctElement());
    _symbolTable = new XsymbolTable(symbols);
    _declTable = new Hashtable<String, XvarDecl>();

    readDeclarationTable();
  }

  public XsymbolTable getSymbolTable(){
    return _symbolTable;
  }

  public Hashtable<String, XvarDecl> getDeclarationTable(){
    return _declTable;
  }

  public Node clone(){
    return getFctElement().cloneNode(true);
  }

  public void addDeclaration(XvarDecl decl){
    _declarationsElement.appendChild(decl.clone());
  }

  private void readDeclarationTable(){
    _declarationsElement = XelementHelper.findDeclarations(getFctElement());
    NodeList nodeList = _declarationsElement
      .getElementsByTagName(XelementName.VAR_DECL);
    for (int i = 0; i < nodeList.getLength(); i++) {
      Node n = nodeList.item(i);
      if (n.getNodeType() == Node.ELEMENT_NODE) {
        Element el = (Element)n;
        XvarDecl decl = new XvarDecl(el);
        _declTable.put(decl.getName(), decl);
      }
    }
  }

  public Element getBody(){
    return XelementHelper.getBody(getFctElement());
  }
}
