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
  private XdeclTable _declTable;

  //private Element _symbolsElement = null;
  private Element _declarationsElement = null;

  public XfctDef(Element fctDefElement){
    super(fctDefElement);
    Element symbols = XelementHelper.findSymbols(getFctElement());
    _symbolTable = new XsymbolTable(symbols);
    Element decl = XelementHelper.findDeclarations(getFctElement());
    _declTable = new XdeclTable(decl);


  }

  public XsymbolTable getSymbolTable(){
    return _symbolTable;
  }

  public XdeclTable getDeclarationTable(){
    return _declTable;
  }

  public Node clone(){
    return getFctElement().cloneNode(true);
  }


  public Element getBody(){
    return XelementHelper.getBody(getFctElement());
  }
}
