/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */
package claw.wani.language;

/**
 * Enumeration representing the clauses in the CLAW directive language. Some
 * clauses are hidden but reflected in this enumeration.
 *
 * @author clementval
 */
public enum ClawClause {
    ACC, COLLAPSE, DATA, DATA_OVER, DIMENSION, FUSION, GROUP, INDUCTION, INIT, INTERCHANGE, INTERCHANGE_INDEXES,
    HOIST_INDUCTIONS, PARALLEL, PRIVATE, RESHAPE, ROUTINE, FORWARD, COPY, UPDATE, TARGET, CONSTRAINT, SAVEPOINT, SCALAR,
    CREATE, CLEANUP, LAYOUT, NO_PROMOTE, FCT_NAME, ARRAY_NAME, FCT_PARAMETERS
}
