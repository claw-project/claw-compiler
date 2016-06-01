package cx2x.translator.xcode;

import cx2x.translator.language.ClawLanguage;
import cx2x.translator.language.helper.accelerator.AcceleratorGenerator;
import cx2x.translator.language.helper.target.Target;
import cx2x.xcodeml.exception.IllegalDirectiveException;
import exc.block.BlockPrintWriter;
import exc.object.*;

/**
 * Xobject translator for the CLAW pragma statements.
 *
 * @author clementval
 */
class ClawTranslate implements XobjectDefVisitor {
  private XobjectFile env;
  private BlockPrintWriter debug_out;
  private final Target _crtTarget;
  private final AcceleratorGenerator _crtGenerator;


  /**
   * Constructs a new ClawTranslate object.
   * @param objectFile Original object file.
   */
  ClawTranslate(XobjectFile objectFile, Target target,
                AcceleratorGenerator generator)
  {
    this.env = objectFile;
    this.debug_out = new BlockPrintWriter(System.out);
    this._crtTarget = target;
    this._crtGenerator = generator;
  }

  /**
   * Action to be applied at the end of the translation.
   */
  void finish(){

  }

  @Override
  public void doDef(XobjectDef xobjectDef) {
    try {
      System.out.println(xobjectDef.getDef().Opcode());
      if (xobjectDef.getDef().Opcode() == Xcode.FUNCTION_DEFINITION) {
        analyzeFunctionBody(xobjectDef.getFuncBody());
      }
    } catch (IllegalDirectiveException idex){
      System.err.println(idex.getMessage());
    }
  }


  /**
   * Analyze the statements list in a function body and trigger the correct
   * translations.
   * @param functionBody
   */
  private void analyzeFunctionBody(Xobject functionBody)
      throws IllegalDirectiveException
  {
    System.out.println("=== ANALYZE FUNCTION BODY ===");
    if(functionBody.Opcode() != Xcode.F_STATEMENT_LIST){
      // Add error
      System.err.print("Cannot analyze function body!");
    }

    topdownXobjectIterator iterator = new topdownXobjectIterator(functionBody);
    iterator.init();

    while(!iterator.end()) {
      Xobject crtObject = iterator.getXobject();
      if(crtObject != null)
        System.out.println(crtObject.Opcode());
      if(crtObject != null) {
        switch (crtObject.Opcode()){
          case PRAGMA_LINE:
            System.out.println(crtObject.getArg(0).getString());
            ClawLanguage l =
                ClawLanguage.analyze(crtObject, _crtGenerator, _crtTarget);
            switch (l.getDirective()){
              case LOOP_FUSION:
                translateLoopFusion(crtObject);
                break;
            }
            break;
          case F_ASSIGN_STATEMENT:
            System.out.println("=== ASSIGN ===");
            break;
        }
      }
      iterator.next();
    }
  }

  private void translateLoopFusion(Xobject pragma){

  }
}
