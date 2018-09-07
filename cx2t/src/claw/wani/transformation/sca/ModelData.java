package claw.wani.transformation.sca;

import claw.shenron.transformation.Transformation;
import claw.shenron.translator.Translator;
import claw.tatsu.common.Utility;
import claw.tatsu.xcodeml.xnode.XnodeUtil;
import claw.tatsu.xcodeml.xnode.common.Xcode;
import claw.tatsu.xcodeml.xnode.common.XcodeProgram;
import claw.tatsu.xcodeml.xnode.common.Xnode;
import claw.tatsu.xcodeml.xnode.fortran.FfunctionDefinition;
import claw.wani.language.ClawPragma;
import claw.wani.transformation.ClawBlockTransformation;
import claw.wani.x2t.translator.ClawTranslator;

import java.util.*;

public class ModelData extends ClawBlockTransformation {

  public ModelData(ClawPragma startDirective, ClawPragma endDirective) {
    super(startDirective, endDirective);
  }

  @Override
  public boolean analyze(XcodeProgram xcodeml, Translator translator) {
    /* Discover variable part of the model configuration and the subroutine
     * holding them */

    ClawTranslator trans = (ClawTranslator) translator;

    // Locate the subroutine/function in which the directive is defined
    FfunctionDefinition sub = getDirective().getPragma().findParentFunction();

    Set<String> modelVariables;
    if(trans.hasElement(sub) != null) {
      modelVariables = Utility.convertToSet(trans.hasElement(sub));
    } else {
      modelVariables = new HashSet<>();
    }

    // Locate all declarations in the model-data block
    List<Xnode> decls = XnodeUtil.getNodes(getDirective().getPragma(),
        getEndDirective().getPragma(),
        Collections.singletonList(Xcode.VAR_DECL));

    // Save variables for SCA usage
    for(Xnode varDecl : decls) {
      Xnode name = varDecl.matchSeq(Xcode.NAME);
      modelVariables.add(name.value());
    }

    trans.storeElement(sub, modelVariables);

    return true;
  }

  @Override
  public boolean canBeTransformedWith(XcodeProgram xcodeml,
                                      Transformation other)
  {
    return false; // Independent transformation
  }

  @Override
  public void transform(XcodeProgram xcodeml, Translator translator,
                        Transformation other)
  {
    // Analysis only transformation. All the work is done in SCA and SCAForward
  }
}
