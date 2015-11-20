package x2x.translator.xcodeml.xelement;

import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;
import java.util.Hashtable;

/**
 * The XfctDef represents the FfunctionDefinition (5.3) element in XcodeML
 * intermediate representation.
 *
 * Elements:
 * - Required:
 *   - name (text)
 *   - body
 * - Optional:
 *   - symbols (XsymbolTable)
 *   - params
 *   - declarations (XdeclTable)
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
