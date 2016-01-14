package x2x.translator.xobject;

import x2x.translator.pragma.CLAWpragma;

import exc.block.*;

public class CLAWblockVisitor implements BasicBlockVisitor {

  public CLAWblockVisitor() {

  }

  public void visit(BasicBlock bb){
    //System.out.println("Visit BB");
    if(bb == null) {
      return;
    }
    for(Statement s = bb.getHead(); s != null; s = s.getNext()) {
      if(s.getExpr() != null) {
        if(s.getExpr().isPragma()){
          String pragmaName = s.getExpr().getArg(0).getString();
          /*System.out.println("PRAGMA FOUND");
          System.out.println("@: " + s.getExpr());
          System.out.println(pragmaName);*/

          if(!CLAWpragma.isValid(pragmaName)){
              System.err.println("Unvalid CLAW pragma detected: !$" + pragmaName);
          } else {
            CLAWpragma directive = CLAWpragma.getDirective(pragmaName);
            if(directive == null){

            } else {
              switch(directive){
                case LOOP_FUSION:
                  System.out.println("Apply loop fusion");
                  break;
                case LOOP_INTERCHANGE:
                  System.out.println("Apply loop interchange");
                  break;
              }
            }
          }
        }
      }
    }
    /*if(bb.getExpr() != null) {
        System.out.println("@: " + bb.getExpr());
    }*/
  }

  public void visit(Block b){

    if(b == null) {
      return;
    }
    if(b instanceof PragmaBlock) {
      PragmaBlock pb = (PragmaBlock)b;
      System.out.println(pb.getPragma());
      //println(" pramga=" + pb.getPragma());
      //println(" args=" + pb.getClauses());
    }

    b.visitBasicBlock(this);
    b.visitBody(this);

  }

  public void visit(BlockList b_list){
    System.out.println("Visit BL");
    if(b_list == null){
      return;
    }
    for(Block b = b_list.getHead(); b != null; b = b.getNext()){
      visit(b);
    }

  }
}
