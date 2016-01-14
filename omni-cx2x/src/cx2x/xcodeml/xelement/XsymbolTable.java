package cx2x.xcodeml.xelement;

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
public class XsymbolTable extends XbaseElement {

  private Hashtable<String, Xid> _table;

  public XsymbolTable(Element symbolsElement){
    super(symbolsElement);
    _table = new Hashtable<String, Xid>();
    readTable();
  }

  private void readTable(){
    NodeList nodeList = baseElement.getElementsByTagName(XelementName.ID);
    for (int i = 0; i < nodeList.getLength(); i++) {
      Node idNode = nodeList.item(i);
      if (idNode.getNodeType() == Node.ELEMENT_NODE) {
        Element idElement = (Element) idNode;
        Xid id = new Xid(idElement);
        _table.put(id.getName(), id);
      }
    }
  }

  public int count(){
    return _table.size();
  }

  public void add(Xid id){
    baseElement.appendChild(id.clone());
    _table.put(id.getName(), id);
  }

  public Xid get(String key){
    return _table.get(key);
  }
}
