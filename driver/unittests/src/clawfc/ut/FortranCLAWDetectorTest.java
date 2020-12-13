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
        verifyDetection("!$claw\n");
        verifyDetection("!$claw\r\n");
        verifyDetection("!$claw\r\n");
        verifyDetection("!$CLaw");
        verifyDetection("!$clAW");
        verifyDetection(" \t!$claw");
        verifyDetection(" \t! \t$claw");
        verifyDetection(" \t! \t$claw \t");
        verifyDetection(" \t! \t$claw \t bla bla bla");
        verifyDetection(" \t! \t$claw \t bla &");
        verifyDetection("!$omp claw");
        verifyDetection("!$oMp claw");
        verifyDetection("!$OmP claw");
        verifyDetection(" \t!$omp claw");
        verifyDetection(" \t! \t$omp claw");
        verifyDetection(" \t! \t$omp\t claw");
        verifyDetection(" \t! \t$omp\t claw \t");
        verifyDetection("!$acc claw");
        verifyDetection("!$aCc claw");
        verifyDetection("!$AcC claw");
        verifyDetection(" \t!$acc claw");
        verifyDetection(" \t! \t$acc claw");
        verifyDetection(" \t! \t$acc\t claw");
        verifyDetection(" \t! \t$acc\t claw \t");
    }
}
