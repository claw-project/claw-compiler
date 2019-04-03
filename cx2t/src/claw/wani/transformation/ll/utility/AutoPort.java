/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */
package claw.wani.transformation.ll.utility;

import claw.shenron.transformation.Transformation;
import claw.shenron.translator.Translator;
import claw.tatsu.xcodeml.xnode.common.Xcode;
import claw.tatsu.xcodeml.xnode.common.XcodeProgram;
import claw.tatsu.xcodeml.xnode.common.Xnode;
import claw.tatsu.xcodeml.xnode.fortran.FfunctionDefinition;
import claw.wani.language.ClawPragma;
import claw.wani.transformation.ClawBlockTransformation;

import java.util.ArrayList;
import java.util.LinkedList;
import java.util.List;

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

    List<Xnode> fields = new LinkedList<>();
    for(Xnode s : siblingsInRegion) {
      fields.addAll(s.matchAll(Xcode.F_ARRAY_REF));
    }
    for(Xnode f : fields) {
      System.out.println(f.toString());
      if(xcodeml.getGlobalSymbolsTable().contains(f.firstChild().value())) {
        System.out.println(f.firstChild().value());
      }
    }
  }
}
