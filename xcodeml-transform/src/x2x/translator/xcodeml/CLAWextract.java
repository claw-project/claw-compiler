package x2x.translator.xcodeml;

import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;
import org.w3c.dom.NamedNodeMap;

public class CLAWextract {
  protected Element _pragmaElement = null;
  protected Element _exprStmtElement = null;

  public CLAWextract(Element pragma, Element exprStmt){
    _pragmaElement = pragma;
    _exprStmtElement = exprStmt;
  }
}
