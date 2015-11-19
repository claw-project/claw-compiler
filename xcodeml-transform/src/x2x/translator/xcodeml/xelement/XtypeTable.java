package x2x.translator.xcodeml.xelement;

import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;
import java.util.Hashtable;

public class XtypeTable {

  private Hashtable<String, Xtype> _table;
  private Element _baseElement = null;

  public XtypeTable(Element symbols){
    _baseElement = symbols;
    _table = new Hashtable<String, Xtype>();
    readTable();
  }

  private void readTable(){
    // Read basic type
    NodeList basicTypes = _baseElement.getElementsByTagName(XelementName.BASIC_TYPE);
    for (int i = 0; i < basicTypes.getLength(); i++) {
      Node n = basicTypes.item(i);
      if (n.getNodeType() == Node.ELEMENT_NODE) {
        Element el = (Element) n;
        XbasicType bt = new XbasicType(el);
        _table.put(bt.getType(), bt);
      }
    }
    // Read fct type
    NodeList fctTypes = _baseElement.getElementsByTagName(XelementName.FCT_TYPE);
    for (int i = 0; i < fctTypes.getLength(); i++) {
      Node n = fctTypes.item(i);
      if (n.getNodeType() == Node.ELEMENT_NODE) {
        Element el = (Element) n;
        XfctType ft = new XfctType(el);
        _table.put(ft.getType(), ft);
      }
    }
  }

  public void add(Xtype type){
    _baseElement.appendChild(type.clone());
    _table.put(type.getType(), type);
  }

  public Xtype get(String key){
    return _table.get(key);
  }
}
