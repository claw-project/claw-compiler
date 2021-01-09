/*
 * @author Mikhail Zhigun
 * @copyright Copyright 2020, MeteoSwiss
 */
package clawfc.tests;

public class DriverTest extends clawfc.tests.utils.DriverTestCase
{
    public void testRun() throws Exception
    {
        run(new String[0]);
    }

    public void testHelp() throws Exception
    {
        String[] args = new String[] { "--help" };
        run(args);
    }
}
