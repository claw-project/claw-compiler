package x2x.translator.pragma;

import exc.block.*;
import exc.object.*;
import xcodeml.util.XmOption;

import x2x.translator.xobject.CLAWglobalDecl;

public class CLAWanalyzePragma implements XobjectDefVisitor {
  private CLAWglobalDecl _globalDecl;
  private CLAWanalyzeLocalPragma _localPragmaAnalyzer;

  public CLAWanalyzePragma(CLAWglobalDecl globalDecl) {
    // TODO error handling is C xcodeml is passed
    //if (XmOption.isLanguageC())
      //ACC.fatal("current version only supports Fortran language.");

    _globalDecl = globalDecl;
    _localPragmaAnalyzer = new CLAWanalyzeLocalPragma(globalDecl);
    //_globalPragmaAnalyzer = new ACCanalyzeGlobalPragma(globalDecl);
  }

  public void finalize() {
    //_globalDecl.checkPresentData();
    //_globalDecl.finalize();
  }

  public void doDef(XobjectDef def) {
    System.out.println("CLAWanalyzePragma");
    analyze(def);
  }

  private void analyze(XobjectDef def) {
    if (def.isFuncDef()) {
      FuncDefBlock fd = new FuncDefBlock(def);
      _localPragmaAnalyzer.analyze(fd);
    } else {
      //_globalPragmaAnalyzer.analyze(def);
      return;
    }
  }
}
