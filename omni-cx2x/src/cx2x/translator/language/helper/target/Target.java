/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */

package cx2x.translator.language.helper.target;

import java.util.Arrays;
import java.util.List;

/**
 * Enumeration that define the possible target supported.
 * Currently CPU, GPU and MIC.
 *
 * @author clementval
 */
public enum Target {
  CPU,
  GPU,
  MIC;

  private static final String cpu = "cpu";
  private static final String gpu = "gpu";
  private static final String mic = "mic";

  public static List<String> availableTargets(){
    return Arrays.asList(cpu, gpu, mic);
  }

  public static Target fromString(String value){
    if(value == null){
      return CPU;
    }
    switch (value){
      case cpu:
        return CPU;
      case gpu:
        return GPU;
      case mic:
        return MIC;
      default:
        return CPU;
    }
  }
}
