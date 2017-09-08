/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */

package cx2x.translator.language.base;

/**
 * ClawDirective enumeration represents the different directives available in
 * the CLAW language.
 *
 * @author clementval
 */
public enum ClawDirective {
  ARRAY_TRANSFORM,
  ARRAY_TO_CALL,
  DEFINE,
  IGNORE,
  IF_EXTRACT,
  KCACHE,
  LOOP_FUSION,
  LOOP_INTERCHANGE,
  LOOP_HOIST,
  LOOP_EXTRACT,
  NO_DEP,
  PRIMITIVE,
  PARALLELIZE,
  REMOVE,
  VERBATIM
}
