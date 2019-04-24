/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */
package claw.wani.transformation.sca;

import claw.shenron.transformation.Transformation;
import claw.shenron.translator.Translator;
import claw.tatsu.xcodeml.abstraction.FunctionCall;
import claw.tatsu.xcodeml.xnode.XnodeUtil;
import claw.tatsu.xcodeml.xnode.common.Xattr;
import claw.tatsu.xcodeml.xnode.common.Xcode;
import claw.tatsu.xcodeml.xnode.common.XcodeProgram;
import claw.tatsu.xcodeml.xnode.common.Xnode;
import claw.wani.language.ClawClause;
import claw.wani.language.ClawPragma;

import java.util.List;
import java.util.stream.Collectors;

/**
 * Pre-pass for the SCA forward
 *
 * @author clementval
 */
public class ScaForwardPrePass extends ScaForward {

  /**
   * Constructs a new Sca transformation triggered from a specific
   * pragma.
   *
   * @param directive The directive that triggered the define transformation.
   */
  public ScaForwardPrePass(ClawPragma directive) {
    super(directive);
  }

  @Override
  public void transform(XcodeProgram xcodeml, Translator translator,
                        Transformation other)
  {
    if(_fctType.isElemental() && !_claw.hasClause(ClawClause.ROUTINE)) {
      lockFctCallArguments(_fCall);
      lockFctCallReturn(_fCall);
    }
  }

  /**
   * Lock array references in fct call to not be adapted by further SCA
   * transformation.
   *
   * @param fctCall Function call node.
   */
  private void lockFctCallArguments(FunctionCall fctCall) {
    List<Xnode> nodes = fctCall.arguments().stream()
        .filter(x -> x.is(Xcode.F_ARRAY_REF) || x.is(Xcode.VAR))
        .collect(Collectors.toList());
    for(Xnode node : nodes) {
      node.setBooleanAttribute(Xattr.IS_LOCKED, true);
    }
  }

  /**
   * Lock the return node of the function call.
   *
   * @param fctCall Function call node.
   */
  private void lockFctCallReturn(FunctionCall fctCall) {
    if(_fctType.isFunction()) {
      Xnode fctCallAncestor = fctCall.matchAncestor(Xcode.F_ASSIGN_STATEMENT);
      if(fctCallAncestor == null) {
        return;
      }

      fctCallAncestor.firstChild().setBooleanAttribute(Xattr.IS_LOCKED, true);
    }
  }

}
