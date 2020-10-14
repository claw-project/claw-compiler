/*
 * @author Mikhail Zhigun
 * @copyright Copyright 2020, MeteoSwiss
 */
package clawfc.depscan;

public class FortranException
    extends Exception
{
    int _line;
    int _charPositionInLine;

    public int line() { return _line; }
    public int charPositionInLine() { return _charPositionInLine; }

    public FortranException(String msg, int line, int charPositionInLine)
    {
        super(msg);
        _line = line;
        _charPositionInLine = charPositionInLine;
    }
}
