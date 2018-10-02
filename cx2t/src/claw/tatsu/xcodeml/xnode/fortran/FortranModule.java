/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */
package claw.tatsu.xcodeml.xnode.fortran;

import claw.tatsu.primitive.Xmod;
import claw.tatsu.xcodeml.xnode.common.Xcode;
import claw.tatsu.xcodeml.xnode.common.XcodeML;
import claw.tatsu.xcodeml.xnode.common.XsymbolTable;
import org.w3c.dom.Document;

/**
 * The FortranModule represents the module information produced by the Fortran
 * front-end of OMNI Compiler.
 *
 * @author clementval
 */
public class FortranModule extends XcodeML {

  // FortranModule inner elements
  private final String _path;
  private final String _name;

  private XsymbolTable _identifiers;

  /**
   * Constructs a basic FortranModule object representing the XcodeML module
   * file given in input.
   *
   * @param baseElement XcodeML document.
   * @param name        Name of the module.
   * @param path        Path of the XcodeML module file without the filename.
   */
  public FortranModule(Document baseElement, String name, String path) {
    super(baseElement);
    _name = name;
    _path = path.endsWith("/") ? path : path + "/";
    _identifiers = new XsymbolTable(matchSeq(Xcode.IDENTIFIERS));
  }

  /**
   * Get the path associated with this XcodeML module.
   *
   * @return Path of the module file.
   */
  public String getPath() {
    return _path;
  }

  /**
   * Get the name of the module.
   *
   * @return Name of the module.
   */
  public String getName() {
    return _name;
  }

  /**
   * Get the full path of the module file.
   *
   * @return Xmod file full path.
   */
  public String getFullPath() {
    return _path + _name + Xmod.getSuffix();
  }

  /**
   * Get the identifiers table.
   *
   * @return Identifiers as a symbol table.
   */
  public XsymbolTable getIdentifiers() {
    return _identifiers;
  }
}
