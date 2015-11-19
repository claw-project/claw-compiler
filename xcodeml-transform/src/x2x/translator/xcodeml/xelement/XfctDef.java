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

  private Hashtable<String, Xid> _symbolTable;
  private Hashtable<String, XvarDecl> _declTable;

  private Element _symbolsElement = null;
  private Element _declarationsElement = null;

  public XfctDef(Element fctDefElement){
    super(fctDefElement);
    _symbolTable = new Hashtable<String, Xid>();
    _declTable = new Hashtable<String, XvarDecl>();
    readSymbolsTable();
    readDeclarationTable();
  }

  public Hashtable<String, Xid> getSymbolTable(){
    return _symbolTable;
  }

  public Hashtable<String, XvarDecl> getDeclarationTable(){
    return _declTable;
  }

  public void addSymbol(Xid id){
    _symbolsElement.appendChild(id.clone());
  }

  public Node clone(){
    return getFctElement().cloneNode(true);
  }

  private void readSymbolsTable(){
    _symbolsElement = XelementHelper.findSymbols(getFctElement());
    NodeList nodeList = _symbolsElement.getElementsByTagName(XelementName.ID);
    for (int i = 0; i < nodeList.getLength(); i++) {
      Node idNode = nodeList.item(i);
      if (idNode.getNodeType() == Node.ELEMENT_NODE) {
        Element idElement = (Element) idNode;
        Xid id = new Xid(idElement);
        _symbolTable.put(id.getName(), id);
      }
    }
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
