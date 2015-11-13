package x2x.translator.xobject;

import exc.object.*;
import exc.block.*;
import exc.openmp.OMPpragma;
import xcodeml.util.XmOption;

import java.util.*;

/**
 * XcalableMP AST translator
 */
public class CLAWtranslator implements XobjectDefVisitor
{
  BlockPrintWriter debug_out;
  private XobjectDef		currentDef;

  //private final static String XMP_GENERATED_CHILD = "XMP_GENERATED_CHILD";

  // alorithms

  public CLAWtranslator() {

  }

  public CLAWtranslator(XobjectFile env) {
    //init(env);
  }


  public void doDef(XobjectDef def){
    translate(def);
  }





  private void translate(XobjectDef def) {

    if (!def.isFuncDef()) { // Translate global directives
      Xobject x = def.getDef();
      System.out.println("OPCODE: " + x.Opcode());

      /*switch (x.Opcode()) {
      case XMP_PRAGMA:
        //_translateGlobalPragma.translate(x);
        break;
      case VAR_DECL:
        // for coarray declaration of XMP1.2
        _rewriteExpr.rewriteVarDecl(x, false);   // isLocal=false
        break;
      }
      return;*/
      return;
    }


    // Translate local directives
    FuncDefBlock fd = new FuncDefBlock(def);

    translateLocal(fd);

    // fix subarrays
    //fixSubArrayRef(fd);

    // translate directives
    //_translateLocalPragma.translate(fd);

    // rewrite expressions
    //_rewriteExpr.rewrite(fd);

    /*String funcName = fd.getBlock().getName();
    if(funcName == "main"){
      try{
        add_args_into_main(fd);   // ex) main() -> main(int argc, char **argv)
        create_new_main(fd);      // create new function "xmpc_main"
      } catch (XMPexception e) {
        Block b = fd.getBlock();
        XMP.error(b.getLineNo(), e.getMessage());
      }
    }*/
  }

  private void translateLocal(FuncDefBlock def){
    FunctionBlock fb = def.getBlock();
    currentDef = def.getDef();

    BlockIterator i = new topdownBlockIterator(fb);
    for (i.init(); !i.end(); i.next()) {
      Block b = i.getBlock();
      System.out.println("translateLocal opcode: " + b.Opcode());
      //System.out.println("block: " + b);

      BasicBlock bb = b.getBasicBlock();
      if(bb!=null){
        System.out.println("bb: " + bb);
        for(Statement s = bb.getHead(); s != null; s = s.getNext()) {
          if(s.getExpr() != null) {
            if(s.getExpr().isPragma()){
              String pragmaName = s.getExpr().getArg(0).getString();
              System.out.println("    PRAGMA " + pragmaName);
            }
          }
        } // end for Statement
      }


      if(b.Opcode() == Xcode.PRAGMA_LINE){

      }
      /*if (b.Opcode() ==  Xcode.XMP_PRAGMA) {
        PragmaBlock pb = (PragmaBlock)b;

        try {
  	       translatePragma(pb);
        } catch (Exception e) {
          //XMP.error(pb.getLineNo(), e.getMessage());
          System.err.println("Error in translateLocal: " + pb.getLineNo());
          System.err.println(e.getMessage());
        }
      }*/
    }

    def.Finalize();
  }

}
