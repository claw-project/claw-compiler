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
  OPENMP
}
