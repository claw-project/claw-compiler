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

public class XfctDef extends Xfct implements Xclonable<XfctDef> {

  private XsymbolTable _symbolTable = null;
  private XdeclTable _declTable = null;
  private Xbody _body = null;


  public XfctDef(Element fctDefElement){
    super(fctDefElement);
    Element symbols = XelementHelper.findSymbols(this);
    _symbolTable = new XsymbolTable(symbols);
    Element decl = XelementHelper.findDeclarations(this);
    _declTable = new XdeclTable(decl);
    _body = XelementHelper.findBody(this);
  }

  public XsymbolTable getSymbolTable(){
    return _symbolTable;
  }

  public XdeclTable getDeclarationTable(){
    return _declTable;
  }

  public Xbody getBody(){
    return _body;
  }

  public XfctDef cloneObject(){
    Element clone = (Element)clone();
    return new XfctDef(clone);
  }
}
