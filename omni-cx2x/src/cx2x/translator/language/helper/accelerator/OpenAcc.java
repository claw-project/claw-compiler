/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */

package cx2x.translator.language.helper.accelerator;

/**
 * OpenACC specific accelerator directive generator.
 *
 * @author clementval
 */
class OpenAcc extends AcceleratorGenerator {

  private static final String OPENACC_PREFIX = "acc";
  private static final String OPENACC_PARALLEL = "acc parallel";
  private static final String OPENACC_END_PARALLEL = "acc end parallel";

  @Override
  protected String getStartParellelDirective() {
    return OPENACC_PARALLEL;
  }

  @Override
  protected String getEndParellelDirective() {
    return OPENACC_END_PARALLEL;
  }

  @Override
  protected String getSingleDirective(String clause) {
    return OPENACC_PREFIX + " " + clause;
  }
}
