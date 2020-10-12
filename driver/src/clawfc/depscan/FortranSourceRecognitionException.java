/*
 * @author Mikhail Zhigun
 * @copyright Copyright 2020, MeteoSwiss
 */
package clawfc.depscan;

import clawfc.depscan.parser.*;

public class FortranSourceRecognitionException
    extends Exception
{
    int _line;
    int _charPositionInLine;

    public int line() { return _line; }
    public int charPositionInLine() { return _charPositionInLine; }

    public FortranSourceRecognitionException(String msg, int line, int charPositionInLine)
    {
        super(msg);
        _line = line;
        _charPositionInLine = charPositionInLine;
    }

    public FortranSourceRecognitionException(FortranSourceRecognitionException e)
    {
        super(e);
        _line = e._line;
        _charPositionInLine = e._charPositionInLine;
    }
}
