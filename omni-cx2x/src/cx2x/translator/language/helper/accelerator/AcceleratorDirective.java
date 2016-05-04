package cx2x.translator.language.helper.accelerator;

/**
 * Enumeration that define the possible accelerator directive supported.
 * Currently OpenACC, OpenMP and NONE are available.
 *
 * Created by clementval on 07/04/16.
 */
public enum AcceleratorDirective {
  NONE,
  OPENACC,
  OPENMP;

  private static final String none = "none";
  private static final String openacc = "openacc";
  private static final String openmp = "openmp";

  public static AcceleratorDirective fromString(String value){
    if(value == null){
      return NONE;
    }
    switch (value){
      case none:
        return NONE;
      case openacc:
        return OPENACC;
      case openmp:
        return OPENMP;
      default:
        return NONE;
    }
  }
}

