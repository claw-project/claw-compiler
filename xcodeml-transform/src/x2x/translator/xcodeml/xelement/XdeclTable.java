package x2x.translator.xcodeml.xelement;

import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;
import java.util.Hashtable;

public class XdeclTable {

  private Hashtable<String, XvarDecl> _table;
  private Element _baseElement = null;

  public XdeclTable(Element symbols){
    _baseElement = symbols;
    _table = new Hashtable<String, XvarDecl>();
    readTable();
  }

  private void readTable(){
    NodeList nodeList = _baseElement
      .getElementsByTagName(XelementName.VAR_DECL);
    for (int i = 0; i < nodeList.getLength(); i++) {
      Node n = nodeList.item(i);
      if (n.getNodeType() == Node.ELEMENT_NODE) {
        Element el = (Element)n;
        XvarDecl decl = new XvarDecl(el);
        _table.put(decl.getName(), decl);
      }
    }
  }

  public void addDeclaration(XvarDecl decl){
    _baseElement.appendChild(decl.clone());
    _table.put(decl.getName(), decl);
  }

  public XvarDecl get(String key){
    return _table.get(key);
  }
}
