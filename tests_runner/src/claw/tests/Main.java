/*
 * @author Mikhail Zhigun
 * @copyright Copyright 2021, MeteoSwiss
 */
package claw.tests;

import clawfc.tests.utils.TestsRunner;

public class Main
{
    public static void main(String[] args)
    {
        TestsRunner.main(args, CLAWDirectiveTest.class, CLAWSingleColumnAbstractionTest.class, CLAWSCAFailureTest.class,
                DriverTest.class, LoopsTest.class, OMNITest.class);
    }
}
