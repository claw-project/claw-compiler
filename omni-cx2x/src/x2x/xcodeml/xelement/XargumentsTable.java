package x2x.xcodeml.xelement;

import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;
import java.util.Hashtable;
import java.util.Map;

/**
 * The XargumentsTable represents the arguments (7.5.3) element in XcodeML
 * intermediate representation.
 *
 * Elements:
 * - Optional:
 *   - exprModel
 */

public class XargumentsTable extends XbaseElement {
  // TODO exprModel. For the moment only read var
  private Hashtable<String, XbaseElement> _table;

  public XargumentsTable(Element arguments){
    super(arguments);
    _table = new Hashtable<String, XbaseElement>();
    readTable();
  }

  private void readTable(){
    // Read Var element
    NodeList elements = baseElement.getElementsByTagName(XelementName.VAR);
    for (int i = 0; i < elements.getLength(); i++) {
      Node n = elements.item(i);
      if (n.getNodeType() == Node.ELEMENT_NODE) {
        Element el = (Element) n;
        Xvar var = new Xvar(el);
        _table.put(var.getValue(), var);
      }
    }
  }

  public XbaseElement findArgument(String name){
     return _table.get(name);
  }

  public void replace(Xvar var, XarrayRef arrayRef){
    if(var != null){
      XelementHelper.insertAfter(var, arrayRef);
      var.delete();
    } else {
      // TODO trigger a critical error
    }
  }

  public void add(XarrayRef arrayRef){
    baseElement.appendChild(arrayRef.clone());
    //_table.put(arrayRef.getVar().getValue(), arrayRef); TODO
  }

  public void add(Xvar var){
    baseElement.appendChild(var.clone());
    _table.put(var.getValue(), var);
  }

  public XbaseElement get(String key){
    return _table.get(key);
  }
}
