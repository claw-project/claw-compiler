package x2x.translator.xcodeml.xelement;

import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;
import java.util.Hashtable;

/**
 * The XdeclTable represents the typeTable (5.2) element in XcodeML intermediate
 * representation.
 *
 * Elements:
 * - Optional:
 *   - varDecl (XvarDecl)
 *   - FstructDecl TODO
 *   - externDecl TODO
 *   - FuseDecl TODO
 *   - FuseOnlyDecl TODO
 *   - FinterfaceDecl TODO
 *   - FnamelistDecl TODO
 *   - FequivalenceDecl TODO
 *   - FcommonDecl TODO
 */

public class XdeclTable {

  private Hashtable<String, XvarDecl> _table;
  private Element _baseElement = null;

  public XdeclTable(Element symbols){
    _baseElement = symbols;
    _table = new Hashtable<String, XvarDecl>();
    readTable();
  }

  private void readTable(){
    // Read all varDecl elements
    NodeList nodeList = _baseElement
      .getElementsByTagName(XelementName.VAR_DECL);
    for (int i = 0; i < nodeList.getLength(); i++) {
      Node n = nodeList.item(i);
      if (n.getNodeType() == Node.ELEMENT_NODE) {
        Element el = (Element)n;
        XvarDecl decl = new XvarDecl(el);
        _table.put(decl.getName().getValue(), decl);
      }
    }

    // TODO read FstructDecl elements
    // TODO read externDecl elements
    // TODO read FuseDecl elements
    // TODO read FuseOnlyDecl elements
    // TODO read FinterfaceDecl elements
    // TODO read FnamelistDecl elements
    // TODO read FequivalenceDecl elements
    // TODO read FcommonDecl elements
  }

  public void add(XvarDecl decl){
    _baseElement.appendChild(decl.clone());
    _table.put(decl.getName().getValue(), decl);
  }

  public XvarDecl get(String key){
    return _table.get(key);
  }

  public int count(){
    return _table.size();
  }
}
