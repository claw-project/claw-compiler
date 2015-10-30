package x2x.translator;

import exc.object.*;
import exc.block.*;

/**
 * XcalableMP AST translator
 */
public class CLAWtranslator implements XobjectDefVisitor
{
  BlockPrintWriter debug_out;


  //private final static String XMP_GENERATED_CHILD = "XMP_GENERATED_CHILD";

  // alorithms

  public CLAWtranslator() {
    System.out.println("CLAW translator created");

  }

  public CLAWtranslator(XobjectFile env) {
    //init(env);
  }


  public void doDef(XobjectDef d){

    FuncDefBlock fd = null;
    Boolean is_module = d.isFmoduleDef();
    Boolean is_func = d.isFuncDef();

    if(is_module){
      System.out.println("MODULE found");
    } else if (is_func){
      System.out.println("FUNCTION found");
      Xtype ft = d.getFuncType();
      if(ft != null && ft.isFprogram()) {
        // Do smth to program function
        fd = new FuncDefBlock(d);
        System.out.println("PROGRAM function: " + fd.getBlock().getName());
      } else if(d.getParent() == null) {
        // standalone function
        fd = new FuncDefBlock(d);
        System.out.println(fd.getBlock().getName());

        Block funcBlock = fd.getBlock();
        //BlockPrintWriter printer = new BlockPrintWriter(System.out);
        CLAWblockVisitor printer = new CLAWblockVisitor();
        funcBlock.visitBody(printer);

        /*Block head = funcBody.getHead();
        Xobject obj = head.toXobject();
        if(obj.isPragma()){
          System.out.println("FOUND PRAGMA");
        }*/

        System.out.println("standalone function");
      } else {
        System.err.println("Fotran: unknown decls");
      }
    }








    /*System.out.println("doDef" + d.toString());
    System.out.println("doDef" + d.getDef().toString());

    System.out.println("=== CHILD");
    for(Xobject decl: (XobjList)d.getDef().getArg(2)){
      System.out.println("  doDef" + d.toString());
      System.out.println("  doDef" + d.getDef().toString());
    }

    Xobject pragme = d.getDef().find(String name, int kind);

    if(d.getDef().isPragma()){
      System.out.println("PRAGMA found");
    }*/
  }
}
