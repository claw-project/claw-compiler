/*
 * @author Mikhail Zhigun
 * @copyright Copyright 2020, MeteoSwiss
 */
package clawfc.tests.utils;

import java.io.PrintWriter;
import java.io.StringWriter;

import org.junit.runner.Description;
import org.junit.runner.Result;
import org.junit.runner.notification.Failure;
import org.junit.runner.notification.RunListener;

class ConsoleColors
{
    // Reset
    public static final String RESET = "\033[0m"; // Text Reset

    // Regular Colors
    public static final String BLACK = "\033[0;30m"; // BLACK
    public static final String RED = "\033[0;31m"; // RED
    public static final String GREEN = "\033[0;32m"; // GREEN
    public static final String YELLOW = "\033[0;33m"; // YELLOW
    public static final String BLUE = "\033[0;34m"; // BLUE
    public static final String PURPLE = "\033[0;35m"; // PURPLE
    public static final String CYAN = "\033[0;36m"; // CYAN
    public static final String WHITE = "\033[0;37m"; // WHITE
}

public class TestsExecutionListener extends RunListener
{
    int _numFinished = 0;
    int _numFailed = 0;
    int _numIgnored = 0;

    static void print(String txt)
    {
        System.out.print(ConsoleColors.RESET + txt);
    }

    static void println(String txt, String color)
    {
        System.out.println(color + txt + ConsoleColors.RESET);
    }

    static void println(String txt)
    {
        println(txt, ConsoleColors.RESET);
    }

    static void printGreen(String txt)
    {
        System.out.print(ConsoleColors.GREEN + txt + ConsoleColors.RESET);
    }

    static void printlnGreen(String txt)
    {
        printGreen(txt);
        System.out.println("");
    }

    static void printRed(String txt)
    {
        System.out.print(ConsoleColors.RED + txt + ConsoleColors.RESET);
    }

    static void printlnRed(String txt)
    {
        printRed(txt);
        System.out.println("");
    }

    public void testRunStarted(Description description) throws Exception
    {

        println("\n----------------------------");
        println("Number of tests to execute: " + description.testCount());
        println("----------------------------\n");
    }

    public void testRunFinished(Result result) throws Exception
    {
        println("\n----------------------------");
        println("Number of tests executed: " + result.getRunCount());
        printlnGreen(String.format("Finished: %s", _numFinished));
        if (_numFailed > 0)
        {
            printlnRed(String.format("Failed: %s", _numFailed));
        }
        if (_numIgnored > 0)
        {
            println(String.format("Ignored: %s", _numIgnored));
        }
        println("----------------------------\n");
    }

    public void testStarted(Description description) throws Exception
    {
        println(description.getClassName() + "." + description.getMethodName() + ": STARTED\n");
    }

    public void testFinished(Description description) throws Exception
    {
        ++_numFinished;
        print(description.getClassName() + "." + description.getMethodName() + ": ");
        printlnGreen("FINISHED\n");
    }

    public void testFailure(Failure failure) throws Exception
    {
        ++_numFailed;
        Description description = failure.getDescription();
        Throwable e = failure.getException();
        StringWriter sw = new StringWriter();
        PrintWriter pw = new PrintWriter(sw);
        e.printStackTrace(pw);
        String stackTrace = sw.toString();
        printlnRed(stackTrace);
        print(description.getClassName() + "." + description.getMethodName() + ": ");
        printlnRed("FAILED\n");
    }

    public void testAssumptionFailure(Failure failure)
    {
        ++_numFailed;
        Description description = failure.getDescription();
        printlnRed(failure.getException().toString());
        print(description.getClassName() + "." + description.getMethodName() + ": ");
        printlnRed("FAILED\n");
    }

    public void testIgnored(Description description) throws Exception
    {
        ++_numIgnored;
        println(description.getClassName() + "." + description.getMethodName() + ": IGNORED\n");
    }
}