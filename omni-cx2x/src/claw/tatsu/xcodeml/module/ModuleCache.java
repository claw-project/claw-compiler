/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */
package claw.tatsu.xcodeml.module;

import claw.tatsu.primitive.Module;
import claw.tatsu.xcodeml.exception.IllegalTransformationException;
import claw.tatsu.xcodeml.xnode.fortran.FortranModule;

import java.util.HashMap;
import java.util.Map;

/**
 * Translator interface
 * <p>
 * Translator stores all the transformation to be applied by a translator.
 *
 * @author clementval
 */
public class ModuleCache {

  private final Map<String, FortranModule> _moduleCache;

  /**
   * Constructs a new empty module cache.
   */
  public ModuleCache() {
    _moduleCache = new HashMap<>();
  }

  /**
   * Check whether a module is in the cache.
   *
   * @param moduleName Name of the module.
   * @return True if the module is in the cache. False otherwise.
   */
  public boolean isModuleLoaded(String moduleName) {
    return _moduleCache.containsKey(moduleName.toLowerCase());
  }

  /**
   * Add a module in the cache by its name. If already present, the module is
   * overwritten.
   *
   * @param moduleName Name of the module.
   * @param module     Module object.
   */
  public void add(String moduleName, FortranModule module) {
    if(_moduleCache.containsKey(moduleName.toLowerCase())) {
      _moduleCache.remove(moduleName.toLowerCase());
    }
    _moduleCache.put(moduleName.toLowerCase(), module);
  }

  /**
   * Get a module in the cache by its name.
   *
   * @param moduleName Name of the module.
   * @return The cached module.
   */
  public FortranModule get(String moduleName) {
    return _moduleCache.get(moduleName.toLowerCase());
  }

  /**
   * Write all modules in the cache to files.
   *
   * @param ident Number of spaces used to indent the XML file.
   */
  public void write(int ident)
      throws IllegalTransformationException
  {
    String suffix = Module.getSuffix();
    for(Map.Entry<String, FortranModule> pair : _moduleCache.entrySet()) {
      FortranModule module = pair.getValue();
      String newModuleName = module.getPath() + module.getName() + suffix;
      module.write(newModuleName, ident);
    }
  }
}
