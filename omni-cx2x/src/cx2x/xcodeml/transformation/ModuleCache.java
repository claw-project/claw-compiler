/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */
package cx2x.xcodeml.transformation;

import cx2x.xcodeml.xnode.Xmod;

import java.util.HashMap;
import java.util.Iterator;
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
   * Constructs a new empty module cache.
   */
  public ModuleCache(){
    _moduleCache = new HashMap<>();
  }

  /**
   * Check whether a module is in the cache.
   * @param moduleName Name of the module.
   * @return True if the module is in the cache. False otherwise.
   */
  public boolean isModuleLoaded(String moduleName){
    return _moduleCache.containsKey(moduleName.toLowerCase());
  }

  /**
   * Add a module in the cache by its name. If already present, the module is
   * overwritten.
   * @param moduleName Name of the module.
   * @param module     Module object.
   */
  public void add(String moduleName, Xmod module){
    if(_moduleCache.containsKey(moduleName.toLowerCase())){
      _moduleCache.remove(moduleName.toLowerCase());
    }
    _moduleCache.put(moduleName.toLowerCase(), module);
  }

  /**
   * Get a module in the cache by its name.
   * @param moduleName Name of the module.
   * @return The cached module.
   */
  public Xmod get(String moduleName){
    return _moduleCache.get(moduleName.toLowerCase());
  }

  /**
   * Get an iterator over all the module in the cache.
   * @return A map entry iterator.
   */
  public Iterator<Map.Entry<String, Xmod>> getIterator(){
    return _moduleCache.entrySet().iterator();
  }
}
