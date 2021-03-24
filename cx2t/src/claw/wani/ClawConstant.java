/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */
package claw.wani;

/**
 * Contains common constants values of the CLAW XcodeML to XcodeML translator
 *
 * @author clementval
 */
public class ClawConstant
{

    private ClawConstant()
    {
        // Avoid possible instantiation of this class.
    }

    public static final String EMPTY_STRING = "";

    public static final String DEFAULT_STEP_VALUE = "1";
    public static final int DEFAULT_MAX_COLUMN = 80;
    public static final String CLAW = "claw";
    public static final int INDENT_OUTPUT = 2; // Number of spaces for indent

    public static final String EXTRACTION_SUFFIX = "_extracted";

    public static final String ERROR_PREFIX_INTERNAL = "internal";
}
