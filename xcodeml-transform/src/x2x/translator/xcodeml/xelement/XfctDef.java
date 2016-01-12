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
    _symbolTable = XelementHelper.findSymbols(this);
    _declTable = XelementHelper.findDeclarations(this);
    _body = XelementHelper.findBody(this, false);
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
