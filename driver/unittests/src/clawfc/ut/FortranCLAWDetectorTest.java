/*
 * @author Mikhail Zhigun
 * @copyright Copyright 2020, MeteoSwiss
 */
package clawfc.ut;

import java.io.ByteArrayInputStream;
import java.io.InputStream;
import java.nio.charset.StandardCharsets;

import clawfc.depscan.FortranCLAWDetector;
import junit.framework.TestCase;

public class FortranCLAWDetectorTest extends TestCase
{
    private FortranCLAWDetector detector = null;

    @Override
    protected void setUp() throws Exception
    {
        if (detector == null)
        {
            detector = new FortranCLAWDetector();
        }
    }

    private void verifyDetection(String str) throws Exception
    {
        verifyDetection(str, true);
    }

    private void verifyDetection(String str, boolean res) throws Exception
    {
        InputStream inStrm = new ByteArrayInputStream(str.getBytes(StandardCharsets.US_ASCII));
        assertEquals(detector.run(inStrm), res);
    }

    public void testDetection() throws Exception
    {
        verifyDetection("", false);
        verifyDetection("bla bla bla", false);
        verifyDetection("!$claw");
        verifyDetection("!$CLaw");
        verifyDetection("!$clAW");
        verifyDetection(" \r\t!$claw");
        verifyDetection(" \r\t! \r\t$claw");
        verifyDetection(" \r\t! \r\t$claw \r\t");
        verifyDetection(" \r\t! \r\t$claw \r\t bla bla bla");
        verifyDetection(" \r\t! \r\t$claw \r\t bla &");
        verifyDetection("!$omp claw");
        verifyDetection("!$oMp claw");
        verifyDetection("!$OmP claw");
        verifyDetection(" \r\t!$omp claw");
        verifyDetection(" \r\t! \r\t$omp claw");
        verifyDetection(" \r\t! \r\t$omp\r\t claw");
        verifyDetection(" \r\t! \r\t$omp\r\t claw \r\t");
        verifyDetection("!$acc claw");
        verifyDetection("!$aCc claw");
        verifyDetection("!$AcC claw");
        verifyDetection(" \r\t!$acc claw");
        verifyDetection(" \r\t! \r\t$acc claw");
        verifyDetection(" \r\t! \r\t$acc\r\t claw");
        verifyDetection(" \r\t! \r\t$acc\r\t claw \r\t");
    }
}
