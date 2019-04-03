/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */
package claw.wani.transformation.ll.utility;

import claw.shenron.transformation.Transformation;
import claw.shenron.translator.Translator;
import claw.tatsu.xcodeml.xnode.common.*;
import claw.tatsu.xcodeml.xnode.fortran.FfunctionDefinition;
import claw.wani.language.ClawPragma;
import claw.wani.transformation.ClawBlockTransformation;

import java.util.*;

/**
 * An array access to function call transformation replace the access to an
 * array value by a function call.
 *
 * @author clementval
 */
public class AutoPort extends ClawBlockTransformation {

  private FfunctionDefinition _replaceFct;

  /**
   * ArrayToFctCall ctor.
   *
   * @param directive The directive that triggered the transformation.
   */
  public AutoPort(ClawPragma startDirective, ClawPragma endDirective) {
    super(startDirective, endDirective);
  }

  @Override
  public boolean analyze(XcodeProgram xcodeml, Translator translator) {

    return true; // skeleton
  }

  /**
   * @return Always false as independent transformation are applied one by one.
   * @see Transformation#canBeTransformedWith(XcodeProgram, Transformation)
   */
  @Override
  public boolean canBeTransformedWith(XcodeProgram xcodeml,
                                      Transformation other)
  {
    return false; // independent transformation
  }

  @Override
  public void transform(XcodeProgram xcodeml, Translator translator,
                        Transformation other)
  {
    List<Xnode> siblingsInRegion = new LinkedList<>();
    Xnode current = _clawStart.getPragma().nextSibling();
    while(!current.equals(_clawEnd.getPragma())) {
      siblingsInRegion.add(current);
      current = current.nextSibling();
    }
    if(siblingsInRegion.isEmpty()) {return;}

    Set<String> locArray = new HashSet<>();
    Set<String> otherArray = new HashSet<>();
    for(Xnode sibling : siblingsInRegion) {
     List<Xnode> fields = sibling.matchAll(Xcode.F_ARRAY_REF);
      for(Xnode field : fields) {
        FfunctionDefinition parentFunction =  field.findParentFunction();
        XsymbolTable funcTable = parentFunction.getSymbolTable();
        otherArray.add(field.firstChild().value());
        for(Xnode child : funcTable.children()) {
          String name = field.firstChild().value();
          if(child.firstChild().value().equals(name)) {
            if (child.getAttribute(Xattr.SCLASS).equals("flocal")) {
              System.out.println(name);
              locArray.add(name);
            }
          }
        }
      }
    }

    otherArray.removeAll(locArray);
    System.out.println(locArray.toString());
    System.out.println(otherArray.toString());
  }
}
