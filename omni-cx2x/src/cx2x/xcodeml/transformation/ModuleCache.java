/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */
package cx2x.xcodeml.transformation;

import cx2x.xcodeml.xnode.Xmod;

import java.util.HashMap;
import java.util.Map;


/**
 * Transformer interface
 *
 * Transformer stores all the transformation to be applied by a translator.
 *
 * @author clementval
 */
public class ModuleCache {

  private final Map<String, Xmod> _moduleCache;

  /**
   *
   */
  public ModuleCache(){
    _moduleCache = new HashMap<>();
  }

  /**
   *
   * @param moduleName
   * @return
   */
  public boolean isModuleLoaded(String moduleName){
    return _moduleCache.containsKey(moduleName.toLowerCase());
  }

  /**
   *
   * @param moduleName
   * @param module
   */
  public void add(String moduleName, Xmod module){
    _moduleCache.put(moduleName.toLowerCase(), module);
  }

  /**
   *
   * @param moduleName
   * @return
   */
  public Xmod get(String moduleName){
    return _moduleCache.get(moduleName.toLowerCase());
  }


}
