package x2x.translator.xcodeml.xelement;

import org.w3c.dom.Element;
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

  public XfctDef(Element fctDefElement){
    super(fctDefElement);
    Element symbols = XelementHelper.findSymbols(baseElement);
    _symbolTable = new XsymbolTable(symbols);
    Element decl = XelementHelper.findDeclarations(baseElement);
    _declTable = new XdeclTable(decl);
  }

  public XsymbolTable getSymbolTable(){
    return _symbolTable;
  }

  public XdeclTable getDeclarationTable(){
    return _declTable;
  }

  public Element getBody(){
    return XelementHelper.getBody(baseElement);
  }

  public XfctDef cloneObject(){
    Element clone = (Element)clone();
    return new XfctDef(clone);
  }
}
