package x2x.translator;

import exc.block.Bcons;
import exc.block.BlockList;
import exc.block.FuncDefBlock;
import exc.object.*;


import java.util.*;

public class CLAWglobalDecl{
  private XobjectFile   _env;
  //private Map<String, FuncInfo> funcInfoMap;

  public CLAWglobalDecl(XobjectFile env) {
    _env = env;
    //funcInfoMap = new HashMap<String, FuncInfo>();
  }

  public XobjectFile getEnv() {
    return _env;
  }

}
