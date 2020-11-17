/*
 * @author Mikhail Zhigun
 * @copyright Copyright 2020, MeteoSwiss
 */
package clawfc.ut;

import clawfc.tests.utils.TestsRunner;

public class Main
{
    public static void main(String[] args)
    {
        TestsRunner.main(args,
            FortranBuildInfoTest.class,
            FortranCLAWScannerTest.class,
            FortranCommentsFilterTest.class,
            FortranDepScannerTest.class,
            FortranDepStatementsRecognizerTest.class,
            FortranLineBreaksFilterTest.class,
            UtilsTest.class);
    }
}
