/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 * @author Mikhail Zhigun
 */
package clawfc.tests;

import clawfc.tests.utils.TestsRunner;

public class Main
{
    public static void main(String[] args)
    {
        TestsRunner.main(args, DriverTest.class, PreprocessorTest.class, DepScanTest.class, XmodGenerationTest.class,
                DepGenTest.class, XastGenerationTest.class, TranslationTest.class, DecompilationTest.class,
                SourceReassemblyTest.class);
    }
}
