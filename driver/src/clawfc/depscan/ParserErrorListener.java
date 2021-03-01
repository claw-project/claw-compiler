/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 * @author Mikhail Zhigun
 */
package clawfc.depscan;

import java.util.concurrent.CancellationException;

import org.antlr.v4.runtime.BaseErrorListener;
import org.antlr.v4.runtime.RecognitionException;
import org.antlr.v4.runtime.Recognizer;

public class ParserErrorListener extends BaseErrorListener
{
    public FortranSyntaxException error()
    {
        return _error;
    }

    FortranSyntaxException _error = null;

    public void reset()
    {
        _error = null;
    }

    @Override
    public void syntaxError(Recognizer<?, ?> recognizer, Object offendingSymbol, int line, int charPositionInLine,
            String msg, RecognitionException e)
    {
        _error = new FortranSyntaxException(msg, null, line - 1, charPositionInLine);
        throw new CancellationException("Syntax error");
    }
}
