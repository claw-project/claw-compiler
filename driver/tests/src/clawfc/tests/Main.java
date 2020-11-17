/*
 * @author Mikhail Zhigun
 * @copyright Copyright 2020, MeteoSwiss
 */
package clawfc.tests;

import clawfc.tests.utils.TestsRunner;

public class Main
{
    public static void main(String[] args)
    {
        TestsRunner.main(args, clawfc.tests.DriverTest.class, clawfc.tests.PreprocessorTest.class);
    }
}
