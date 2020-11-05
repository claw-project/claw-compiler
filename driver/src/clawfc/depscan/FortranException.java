/*
 * @author Mikhail Zhigun
 * @copyright Copyright 2020, MeteoSwiss
 */
package clawfc.depscan;

public class FortranException
    extends Exception
{
    Integer _line;
    Integer _charPositionInLine;

    public Integer line() { return _line; }
    public Integer charPositionInLine() { return _charPositionInLine; }
    
    public FortranException(String msg)
    {
        super(msg);
    }

    public FortranException(String msg, int line, int charPositionInLine)
    {
        super(msg);
        _line = line;
        _charPositionInLine = charPositionInLine;
    }
}
