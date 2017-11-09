/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */
package cx2x.translator.transformation.primitive;

import cx2x.xcodeml.helper.XnodeUtil;
import cx2x.xcodeml.xnode.Xmod;
import exc.xcodeml.XcodeMLtools_Fmod;
import org.w3c.dom.Document;

import java.io.File;

/**
 * @author clementval
 */
public final class Module {

  private static final String XMOD_FILE_EXTENSION = ".xmod";

  // Avoid instantiation of this class
  private Module() {
  }

  /**
   * Find module by name.
   *
   * @param moduleName Name of the module.
   * @return Module object if found. Null otherwise.
   */
  public static Xmod find(String moduleName) {
    for(String dir : XcodeMLtools_Fmod.getSearchPath()) {
      String path = dir + "/" + moduleName + XMOD_FILE_EXTENSION;
      File f = new File(path);
      if(f.exists()) {
        Document doc = XnodeUtil.readXmlFile(path);
        return doc != null ? new Xmod(doc, moduleName, dir) : null;
      }
    }
    return null;
  }


}
