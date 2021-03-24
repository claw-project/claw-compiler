/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */
package claw.wani.language;

/**
 * ClawDirective enumeration represents the different directives available in
 * the CLAW language.
 *
 * @author clementval
 */
public enum ClawDirective {
    ARRAY_TO_CALL, DEFINE, EXPAND, IGNORE, IF_EXTRACT, KCACHE, LOOP_FUSION, LOOP_INTERCHANGE, LOOP_HOIST, LOOP_EXTRACT,
    MODEL_DATA, NO_DEP, PRIMITIVE, REMOVE, SCA, VERBATIM
}
