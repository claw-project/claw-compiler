/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */
package claw.tatsu.xcodeml.exception;

/**
 * Exception thrown during the transformation process
 *
 * @author clementval
 */

public class IllegalTransformationException extends Exception
{

    private int _transformationStartLine = 0;

    /**
     * Constructs a new exception with null as its detail message and 0 as line
     * number.
     */
    public IllegalTransformationException()
    {
        super();
    }

    /**
     * Constructs a new exception with null as its detail message and a specific
     * line number.
     *
     * @param lineno Line number in the XcodeML that triggered the exception.
     */
    public IllegalTransformationException(int lineno)
    {
        super();
        _transformationStartLine = lineno;
    }

    /**
     * Constructs a new exception with a specific details message and 0 as line
     * number.
     *
     * @param message Specific exception message.
     */
    public IllegalTransformationException(String message)
    {
        super(message);
    }

    /**
     * Constructs a new exception with a specific detail message and a specific line
     * number.
     *
     * @param message Specific exception message.
     * @param lineno  Line number in the XcodeML that triggered the exception.
     */
    public IllegalTransformationException(String message, int lineno)
    {
        super(message);
        _transformationStartLine = lineno;
    }

    /**
     * Constructs a new exception with a specific detail message and cause.
     *
     * @param message Specific exception message.
     * @param cause   the cause.
     */
    public IllegalTransformationException(String message, Throwable cause)
    {
        super(message, cause);
    }

    /**
     * Constructs a new exception with a specific detail message, cause and line
     * number.
     *
     * @param message Specific exception message.
     * @param cause   the cause
     * @param lineno  Line number in the XcodeML that triggered the exception.
     */
    public IllegalTransformationException(String message, Throwable cause, int lineno)
    {
        super(message, cause);
        _transformationStartLine = lineno;
    }

    /**
     * Constructs a new exception with null as its detail message, 0 as line number
     * and a specific cause.
     *
     * @param cause The cause.
     */
    public IllegalTransformationException(Throwable cause)
    {
        super(cause);
    }

    /**
     * Constructs a new exception with null as its detail message, a specific line
     * number and a specific cause.
     *
     * @param cause  the cause.
     * @param lineno Line number in the XcodeML that triggered the exception.
     */
    public IllegalTransformationException(Throwable cause, int lineno)
    {
        super(cause);
        _transformationStartLine = lineno;
    }

    /**
     * Get the start line of the transformation that triggered the exception.
     *
     * @return Line number in the XcodeML that triggered the exception.
     */
    public int getStartLine()
    {
        return _transformationStartLine;
    }

    /**
     * Set the start line of the transformation that triggered the exception.
     *
     * @param lineno Line number in the XcodeML that triggered the exception.
     */
    public void setStartLine(int lineno)
    {
        _transformationStartLine = lineno;
    }
}
