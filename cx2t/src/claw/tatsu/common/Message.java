/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */
package claw.tatsu.common;

import java.io.PrintStream;
import java.util.List;

import claw.tatsu.xcodeml.error.XanalysisError;
import claw.tatsu.xcodeml.xnode.common.XcodeProgram;

/**
 * Centralized compiler output messages.
 *
 * @author clementval
 */
public final class Message
{

    private static final String ERROR_PREFIX = "error:";
    private static final String WARNING_PREFIX = "warning:";

    // Avoid potential instantiation of this class
    private Message()
    {
    }

    /**
     * Display debug message if debugging is activated.
     *
     * @param message Message to display.
     */
    public static void debug(Context context, String message)
    {
        if (context.getXmOption().isDebugOutput())
        {
            context.getErrorStream().println(message);
        }
    }

    /**
     * Print all messages in the given list with the prefix.
     *
     * @param originalFile Original file name.
     * @param prefix       Prefix for the message.
     * @param messages     List of messages to display.
     */
    private static void printMessages(Context context, String originalFile, String prefix,
            List<XanalysisError> messages)
    {
        for (XanalysisError message : messages)
        {
            PrintStream err = context.getErrorStream();
            if (message.getLine() == 0)
            {
                err.println(String.format("%s:-:- %s %s", originalFile, prefix, message.getMessage()));
            } else
            {
                err.println(String.format("%s:%s:- %s %s", originalFile, message.getConcatLines(), prefix,
                        message.getMessage()));
            }
        }
        messages.clear();
    }

    /**
     * Display all warnings stored in translation unit.
     *
     * @param translationUnit Current translation unit.
     */
    public static void warnings(Context context, XcodeProgram translationUnit)
    {
        if (translationUnit != null)
        {
            printMessages(context, translationUnit.getSourceFileOnly(), WARNING_PREFIX, translationUnit.getWarnings());
        }
    }

    /**
     * Display all errors stored in translation unit.
     *
     * @param translationUnit Current translation unit.
     */
    public static void errors(Context context, XcodeProgram translationUnit)
    {
        if (translationUnit != null)
        {
            printMessages(context, translationUnit.getSourceFileOnly(), ERROR_PREFIX, translationUnit.getErrors());
        }
    }
}
