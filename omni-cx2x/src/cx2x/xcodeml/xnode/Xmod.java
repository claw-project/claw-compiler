/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */

package cx2x.xcodeml.xnode;

import org.w3c.dom.Document;


/**
 * The Xmod represents the module information produced by the Fortran front-end
 * of OMNI Compiler.
 *
 * @author clementval
 */
public class Xmod extends Xnode {

  // Xmod inner elements
  private final String _path;
  private final XtypeTable _typeTable;

  public Xmod(Document baseElement, String path){
    super(baseElement.getDocumentElement());
    _typeTable = new XtypeTable(find(Xcode.TYPETABLE).getElement());
    _path = path;
  }

  /**
   * Get the type table of the Xmod module.
   * @return The types table.
   */
  public XtypeTable getTypeTable(){
    return _typeTable;
  }
}
