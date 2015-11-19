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
    // TODO
  }

  public void add(Xtype type){
    _baseElement.appendChild(type.clone());
    _table.put(type.getType(), type);
  }

  public Xtype get(String key){
    return _table.get(key);
  }
}
