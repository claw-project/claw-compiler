package x2x.translator.xcodeml.xelement;

import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;
import java.util.Hashtable;

public class XsymbolTable {

  private Hashtable<String, Xid> _table;
  private Element _symbolsElement = null;

  public XsymbolTable(Element symbols){
    _symbolsElement = symbols;
    _table = new Hashtable<String, Xid>();
    readSymbolsTable();
  }

  private void readSymbolsTable(){
    NodeList nodeList = _symbolsElement.getElementsByTagName(XelementName.ID);
    for (int i = 0; i < nodeList.getLength(); i++) {
      Node idNode = nodeList.item(i);
      if (idNode.getNodeType() == Node.ELEMENT_NODE) {
        Element idElement = (Element) idNode;
        Xid id = new Xid(idElement);
        _table.put(id.getName(), id);
      }
    }
  }

  public void addSymbol(Xid id){
    _symbolsElement.appendChild(id.clone());
  }

  public Xid get(String key){
    return _table.get(key);
  }
}
