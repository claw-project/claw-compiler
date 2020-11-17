/*
 * @author Mikhail Zhigun
 * @copyright Copyright 2020, MeteoSwiss
 */
package clawfc.tests;

import org.junit.runner.Description;
import org.junit.runner.JUnitCore;
import org.junit.runner.Request;
import org.junit.runner.Result;
import org.junit.runner.manipulation.Filter;

public class Main
{
    public static void main(String[] args)
    {
        JUnitCore junitCore = new JUnitCore();
        Class<?>[] TEST_CASES = new Class[] { DriverTest.class, PreprocessorTest.class };
        Request request = Request.classes(TEST_CASES);
        junitCore.addListener(new TestsExecutionListener());
        if (args.length > 0)
        {
            final String TEST_CLASS_NAME = args[0];
            final String TEST_METHOD_NAME = args.length > 1 ? args[1] : null;
            Filter filter = Filter
                    .matchMethodDescription(Description.createTestDescription(TEST_CLASS_NAME, TEST_METHOD_NAME));
            request = request.filterWith(filter);
        }
        Result result = junitCore.run(request);
        if (result.wasSuccessful())
        {
            System.exit(0);
        } else
        {
            System.exit(1);
        }
    }
}
