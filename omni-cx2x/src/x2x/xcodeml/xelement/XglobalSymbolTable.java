package x2x.xcodeml.xelement;

import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;
import java.util.Hashtable;

/**
 * The XglobalSymbolTable represents the globalSymbols (4.1) element in
 * XcodeML intermediate representation.
 *
 * Implementation is done in XsymbolTable as they are identical. Just the
 * root element name differs.
 *
 * Elements:
 * - Optional:
 *   - id
 */
 
public class XglobalSymbolTable extends XsymbolTable {

  public XglobalSymbolTable(Element baseElement){
    super(baseElement);
  }

}
