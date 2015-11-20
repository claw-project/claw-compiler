package x2x.translator.xcodeml.xelement;

import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;
import java.util.Hashtable;

/**
 * The XsymbolTable represents the symbols (4) element in XcodeML intermediate
 * representation.
 *
 * Elements:
 * - Optional:
 *   - id
 */
public class XsymbolTable {

  private Hashtable<String, Xid> _table;
  private Element _baseElement = null;

  public XsymbolTable(Element symbols){
    _baseElement = symbols;
    _table = new Hashtable<String, Xid>();
    readTable();
  }

  private void readTable(){
    NodeList nodeList = _baseElement.getElementsByTagName(XelementName.ID);
    for (int i = 0; i < nodeList.getLength(); i++) {
      Node idNode = nodeList.item(i);
      if (idNode.getNodeType() == Node.ELEMENT_NODE) {
        Element idElement = (Element) idNode;
        Xid id = new Xid(idElement);
        _table.put(id.getName(), id);
      }
    }
  }

  public void add(Xid id){
    _baseElement.appendChild(id.clone());
    _table.put(id.getName(), id);
  }

  public Xid get(String key){
    return _table.get(key);
  }
}
