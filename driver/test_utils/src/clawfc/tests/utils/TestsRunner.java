/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 * @author Mikhail Zhigun
 */
package clawfc.tests.utils;

import org.junit.runner.Description;
import org.junit.runner.JUnitCore;
import org.junit.runner.Request;
import org.junit.runner.Result;
import org.junit.runner.manipulation.Filter;

public class TestsRunner
{
    public static void main(String[] args, Class<?>... testCases)
    {
        JUnitCore junitCore = new JUnitCore();
        Request request = Request.classes(testCases);
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