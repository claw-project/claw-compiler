/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 * @author Mikhail Zhigun
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
