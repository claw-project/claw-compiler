package x2x.translator.xcodeml.xelement;

import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;
import java.util.Hashtable;

/**
 * The XargumentsTable represents the arguments (7.5.3) element in XcodeML
 * intermediate representation.
 *
 * Elements:
 * - Optional:
 *   - exprModel
 */

public class XargumentsTable {
  // TODO exprModel. For the moment only read var
  private Hashtable<String, Xvar> _table;
  private Element _baseElement = null;

  public XargumentsTable(Element arguments){
    _baseElement = arguments;
    _table = new Hashtable<String, Xvar>();
    readTable();
  }

  private void readTable(){
    // Read Var element
    NodeList elements = _baseElement.getElementsByTagName(XelementName.VAR);
    for (int i = 0; i < elements.getLength(); i++) {
      Node n = elements.item(i);
      if (n.getNodeType() == Node.ELEMENT_NODE) {
        Element el = (Element) n;
        Xvar var = new Xvar(el);
        _table.put(var.getType(), var);
      }
    }
  }

  public void add(Xvar var){
    _baseElement.appendChild(var.clone());
    _table.put(var.getType(), var);
  }

  public Xvar get(String key){
    return _table.get(key);
  }
}
