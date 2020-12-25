/*
 * @author Mikhail Zhigun
 * @copyright Copyright 2020, MeteoSwiss
 */
package clawfc.utils;

import java.util.concurrent.CancellationException;

import org.antlr.v4.runtime.BaseErrorListener;
import org.antlr.v4.runtime.RecognitionException;
import org.antlr.v4.runtime.Recognizer;

public class ParserErrorListener extends BaseErrorListener
{
    public Exception error()
    {
        return _error;
    }

    Exception _error = null;

    public void reset()
    {
        _error = null;
    }

    @Override
    public void syntaxError(Recognizer<?, ?> recognizer, Object offendingSymbol, int line, int charPositionInLine,
            String msg, RecognitionException e)
    {
        _error = e;
        throw new CancellationException("Syntax error");
    }
}
