package x2x.translator;

import exc.block.*;

public class CLAWblockVisitor implements BasicBlockVisitor {

  public CLAWblockVisitor() {

  }

  public void visit(BasicBlock bb){
    System.out.println("Visit BB");
    if(bb == null) {
      return;
    }
    for(Statement s = bb.getHead(); s != null; s = s.getNext()) {
      if(s.getExpr() != null) {
          System.out.println("@: " + s.getExpr());
      }
    }
    if(bb.getExpr() != null) {
        System.out.println("@: " + bb.getExpr());
    }
  }

  public void visit(Block b){
    System.out.println("Visit B");
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
