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
public class Xmod extends XcodeML {

  // Xmod inner elements
  private final String _path;
  private final String _name;

  /**
   * Constructs a basic Xmod object representing the XcodeML module file given
   * in input.
   * @param baseElement XcodeML document.
   * @param name        Name of the module.
   * @param path        Path of the XcodeML module file without the filename.
   */
  public Xmod(Document baseElement, String name, String path){
    super(baseElement);
    _name = name;
    _path = path.endsWith("/") ? path : path + "/";
  }

  /**
   * Get the path associated with this XcodeML module.
   * @return Path of the module file.
   */
  public String getPath(){
    return _path;
  }

  /**
   * Get the name of the module.
   * @return Name of the module.
   */
  public String getName() {
    return _name;
  }
}
