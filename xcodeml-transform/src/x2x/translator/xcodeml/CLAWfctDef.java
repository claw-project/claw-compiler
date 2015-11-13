package x2x.translator.xcodeml;

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

public class CLAWfctDef extends CLAWfct {

  private Hashtable<String, CLAWid> _symbolTable;
  private Hashtable<String, CLAWvarDecl> _declTable;

  private Element _symbolsElement = null;
  private Element _declarationsElement = null;

  public CLAWfctDef(Element fctDefElement){
    super(fctDefElement);
    _symbolTable = new Hashtable<String, CLAWid>();
    _declTable = new Hashtable<String, CLAWvarDecl>();
    readSymbolsTable();
    readDeclarationTable();
  }

  public Hashtable<String, CLAWid> getSymbolTable(){
    return _symbolTable;
  }

  public Hashtable<String, CLAWvarDecl> getDeclarationTable(){
    return _declTable;
  }

  public void addSymbol(CLAWid id){
    _symbolsElement.appendChild(id.clone());
  }

  private void readSymbolsTable(){
    _symbolsElement = CLAWelementHelper.findSymbols(getFctElement());
    NodeList nodeList = _symbolsElement.getElementsByTagName(XelementName.ID);
    for (int i = 0; i < nodeList.getLength(); i++) {
      Node idNode = nodeList.item(i);
      if (idNode.getNodeType() == Node.ELEMENT_NODE) {
        Element idElement = (Element) idNode;
        CLAWid id = new CLAWid(idElement);
        _symbolTable.put(id.getName(), id);
      }
    }
  }

  public void addDeclaration(CLAWvarDecl decl){
    _declarationsElement.appendChild(decl.clone());
  }

  private void readDeclarationTable(){
    _declarationsElement = CLAWelementHelper.findDeclarations(getFctElement());
    NodeList nodeList = _declarationsElement
      .getElementsByTagName(XelementName.VAR_DECL);
    for (int i = 0; i < nodeList.getLength(); i++) {
      Node n = nodeList.item(i);
      if (n.getNodeType() == Node.ELEMENT_NODE) {
        Element el = (Element)n;
        CLAWvarDecl decl = new CLAWvarDecl(el);
        _declTable.put(decl.getName(), decl);
      }
    }
  }

  public Element getBody(){
    return CLAWelementHelper.getBody(getFctElement());
  }
}
