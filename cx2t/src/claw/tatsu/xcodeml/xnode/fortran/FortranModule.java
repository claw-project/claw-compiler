/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */
package claw.tatsu.xcodeml.xnode.fortran;

import claw.tatsu.primitive.Function;
import claw.tatsu.primitive.Xmod;
import claw.tatsu.xcodeml.xnode.common.*;
import org.w3c.dom.Document;

import java.util.HashSet;
import java.util.List;
import java.util.Set;

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

  private final XsymbolTable _identifiers;

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

  /**
   * Check if the given name is define as an interface.
   *
   * @param interfaceName Interface name to check for.
   * @return True if the interface is defined in the current module.
   * False otherwise.
   */
  private boolean isInterfaceDeclaration(String interfaceName) {
    List<Xnode> interfaceDecls = matchAll(Xcode.F_INTERFACE_DECL);
    for(Xnode interfaceDecl : interfaceDecls) {
      if(interfaceDecl.getAttribute(Xattr.NAME) != null
          && interfaceDecl.getAttribute(Xattr.NAME).
          equalsIgnoreCase(interfaceName))
      {
        return true;
      }
    }
    return false;
  }

  /**
   * Find the correct implementation of a generic call.
   *
   * @param fctCall Actual function call.
   * @return Matched function type if can be found. Null otherwise.
   */
  private FfunctionType findFunctionTypeMatchingFctCall(Xnode fctCall) {
    String fctName = Function.getFctNameFromFctCall(fctCall);
    if(fctName == null) {
      return null;
    }
    Set<String> fctTypes = getInterfaceImplementation(fctName);
    int nbArgs = Function.getNbOfArgsFromFctCall(fctCall);
    for(String type : fctTypes) {
      FfunctionType tmp = getTypeTable().getFunctionType(type);
      if(tmp != null) {
        int nbParams = 0;
        for(Xnode param : tmp.getParameters()) {
          if(!param.hasAttribute(Xattr.IS_INSERTED)) {
            ++nbParams;
          }
        }
        // TODO several function might have same number of args.
        if(nbArgs == nbParams) {
          return tmp;
        }
      }
    }
    return null;
  }

  /**
   * Gather all the function type hash that implement the interface.
   *
   * @param interfaceName Interface name.
   * @return Set of all type hashes.
   */
  private Set<String> getInterfaceImplementation(String interfaceName) {
    Set<String> fctTypes = new HashSet<>();
    List<Xnode> interfaceDecls = matchAll(Xcode.F_INTERFACE_DECL);
    for(Xnode interfaceDecl : interfaceDecls) {
      if(interfaceDecl.getAttribute(Xattr.NAME) != null
          && interfaceDecl.getAttribute(Xattr.NAME).
          equalsIgnoreCase(interfaceName))
      {
        List<Xnode> moduleProcDecls =
            interfaceDecl.matchAll(Xcode.F_MODULE_PROCEDURE_DECL);
        for(Xnode moduleProcDecl : moduleProcDecls) {
          for(Xnode name : moduleProcDecl.matchAll(Xcode.NAME)) {
            fctTypes.add(name.getAttribute(Xattr.TYPE));
          }
        }
      }
    }
    return fctTypes;
  }

  /**
   * Try to find the function type in the module.
   *
   * @param fctCall Function call node.
   * @return Function type if found. Null otherwise.
   */
  public FfunctionType findFunctionTypeFromCall(Xnode fctCall) {
    String fctName = Function.getFctNameFromFctCall(fctCall);
    if(fctName == null) {
      return null;
    }
    FfunctionType fctType = findFunctionType(fctName);

    // Make sure it is not the generic function type for the interface
    if(fctType != null && fctType.getParameters().isEmpty()
        && isInterfaceDeclaration(fctName))
    {
      fctType = findFunctionTypeMatchingFctCall(fctCall);
    }
    return fctType;
  }

  /**
   * Try to find the function type in the module.
   *
   * @param fctName Function name.
   * @return Function type if found. Null otherwise.
   */
  public FfunctionType findFunctionType(String fctName) {
    Xid id = getIdentifiers().get(fctName);
    FfunctionType fctType;
    if(id != null) {
      fctType = getTypeTable().getFunctionType(id);
    } else {
      String typeHash = findFunctionImplementationInInterface(fctName);
      fctType = getTypeTable().getFunctionType(typeHash);
    }
    return fctType;
  }

  /**
   * Try to find the function type hash in the module procedure of interfaces
   * in the module.
   *
   * @param name Name of the function to be found.
   * @return Type hash if found. Null otherwise.
   */
  private String findFunctionImplementationInInterface(String name) {
    List<Xnode> interfaceDeclarations = matchAll(Xcode.F_INTERFACE_DECL);
    for(Xnode interfaceDeclaration : interfaceDeclarations) {
      for(Xnode moduleProcDeclaration :
          interfaceDeclaration.matchAll(Xcode.F_MODULE_PROCEDURE_DECL)) {
        for(Xnode nameNode : moduleProcDeclaration.matchAll(Xcode.NAME)) {
          if(nameNode.value().equalsIgnoreCase(name)) {
            return nameNode.getAttribute(Xattr.TYPE);
          }
        }
      }
    }
    return null;
  }
}
