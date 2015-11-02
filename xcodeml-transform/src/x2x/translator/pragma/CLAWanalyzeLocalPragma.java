package x2x.translator.pragma;

import exc.block.*;
import exc.object.*;

import x2x.translator.xobject.CLAWglobalDecl;

import java.util.*;


public class CLAWanalyzeLocalPragma {
  private CLAWglobalDecl _globalDecl;

  public CLAWanalyzeLocalPragma(CLAWglobalDecl globalDecl) {
    _globalDecl = globalDecl;
  }

  public void analyze(FuncDefBlock def) {
    System.out.println("CLAWanalyzeLocalPragma");
    System.out.println(def.getBlock().toString());
    FunctionBlock fb = def.getBlock();

    BlockIterator i = new topdownBlockIterator(fb);
    for (i.init(); !i.end(); i.next()) {
      Block b = i.getBlock();



      System.out.println("  CLAWanalyzeLocalPragma Block " + b.Opcode());
      //System.out.println(b.toString());
      if (b.Opcode() ==  Xcode.PRAGMA_LINE) {
        PragmaBlock pb = (PragmaBlock)b;
        try {
          analyzePragma(pb);
        } catch (Exception e) {
          //TODO
          //ACC.error(pb.getLineNo(), e.getMessage());
        }
      } else if (b.Opcode() == Xcode.F_STATEMENT_LIST){
        System.out.println("    F_STATEMENT_LIST");
        //System.out.println(b.toString());
        BlockIterator j = new topdownBlockIterator(b);
        for (j.init(); !j.end(); j.next()) {
          BasicBlock bb = j.getBlock().getBasicBlock();
          if(bb != null){
            for(Statement s = bb.getHead(); s != null; s = s.getNext()) {
              if(s.getExpr() != null) {
                if(s.getExpr().isPragma()){
                  String pragmaName = s.getExpr().getArg(0).getString();
                  System.out.println("    PRAGMA " + pragmaName);
                }
              }
            } // end for Statement
          }

        } // end for BlockIterator
      }
    }
  }

  private void analyzePragma(PragmaBlock pb) /* throws ACCexception */ {
    String pragmaName = pb.getPragma();
    System.out.println("CLAW PRAGMA: " + CLAWpragma.valueOf(pragmaName));
    /*switch (ACCpragma.valueOf(pragmaName)) {
    case PARALLEL:
      analyzeParallel(pb); break;
    case KERNELS:
      analyzeKernels(pb); break;
    case DATA:
      analyzeData(pb); break;
    case HOST_DATA:
      analyzeHostData(pb); break;
    case LOOP:
      analyzeLoop(pb); break;
    case CACHE:
      analyzeCache(pb); break;
    case PARALLEL_LOOP:
      analyzeParallelLoop(pb); break;
    case KERNELS_LOOP:
      analyzeKernelsLoop(pb); break;
    case DECLARE:
      analyzeDeclare(pb); break;
    case UPDATE:
      analyzeUpdate(pb); break;
    case WAIT:
      analyzeWait(pb); break;
    case ENTER_DATA:
      analyzeEnterData(pb); break;
    case EXIT_DATA:
      analyzeExitData(pb); break;

    default:
      throw new ACCexception("'" + pragmaName.toLowerCase() + "' directive is not supported yet");
    }*/
  }

}
