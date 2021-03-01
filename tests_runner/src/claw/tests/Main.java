/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 * @author Mikhail Zhigun
 */
package claw.tests;

import clawfc.tests.utils.TestsRunner;

public class Main
{
    public static void main(String[] args)
    {
        TestsRunner.main(args, CLAWDirectiveTest.class, CLAWSingleColumnAbstractionTest.class, CLAWSCAFailureTest.class,
                DriverTest.class, LoopsTest.class, OMNITest.class, OpenACCTest.class, OpenMPTest.class,
                UtilitiesTest.class);
    }
}
