/*
 * @author Mikhail Zhigun
 * @copyright Copyright 2020, MeteoSwiss
 */
package clawfc.depscan;

public class FortranSemanticException extends FortranException
{
    public FortranSemanticException(String msg, int line, int charPositionInLine)
    {
        super(msg, line, charPositionInLine);
    }

    public FortranSemanticException(String msg)
    {
        super(msg);
    }
}
