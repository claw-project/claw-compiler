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
        TestsRunner.main(args, DriverTest.class, PreprocessorTest.class, DepScanTest.class, XmodGenerationTest.class,
                XastGenerationTest.class, TranslationTest.class, DecompilationTest.class, SourceReassemblyTest.class);
    }
}
