/*
 * @author Mikhail Zhigun
 * @copyright Copyright 2020, MeteoSwiss
 */
package clawfc.ut;

import java.io.InputStream;
import java.nio.file.Files;
import java.nio.file.Path;

import clawfc.depscan.FortranIncludeChecker;
import clawfc.depscan.FortranIncludeStatementRecognizer;
import clawfc.utils.ByteArrayIOStream;
import junit.framework.TestCase;

public class FortranIncludeTest extends TestCase
{
    protected final Path RES_DIR = clawfc.ut.Resources.DIR;

    public void testChecker() throws Exception
    {
        final Path IN_DIR = RES_DIR.resolve("include_filter/input");
        final Path IN_FILEPATH = IN_DIR.resolve("1.f90");

        InputStream in = Files.newInputStream(IN_FILEPATH);
        ByteArrayIOStream out = new ByteArrayIOStream();
        FortranIncludeChecker filter = new FortranIncludeChecker();
        assertTrue(filter.run(in));
    }

    void verifyParsing(String incLine, String incString) throws Exception
    {
        FortranIncludeStatementRecognizer parser = new FortranIncludeStatementRecognizer();
        String res = parser.run(incLine);
        assertEquals(incString, res);
    }

    public void testParser() throws Exception
    {
        verifyParsing("include \"file.inc\"\n", "file.inc");
        verifyParsing(" \r\tinclude \"file.inc\"\n", "file.inc");
        verifyParsing(" \r\tinclude \r\t\"file.inc\"\n", "file.inc");
        verifyParsing(" \r\tinclude \r\t\"file.inc\"\r\t \n", "file.inc");
        verifyParsing(" \r\tinclude \r\t\"\"\"file.inc\"\r\t \n", "\"file.inc");
        verifyParsing(" \r\tinclude \r\t'file.inc'\r\t \n", "file.inc");
        verifyParsing(" \r\tinclude \r\t'''file.inc'\r\t \n", "'file.inc");
    }

}
