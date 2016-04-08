/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */

package cx2x.translator.language.helper.accelerator;

import sun.reflect.generics.reflectiveObjects.NotImplementedException;

/**
 * OpenMP specific accelerator directive generator.
 *
 * TODO all
 *
 * @author clementval
 */
class OpenMp extends AcceleratorGenerator {
  @Override
  protected String getStartParellelDirective() {
    throw new NotImplementedException();
  }

  @Override
  public String getEndParellelDirective() {
    throw new NotImplementedException();
  }

  @Override
  public String getSingleDirective(String clause) {
    throw new NotImplementedException();
  }
}
