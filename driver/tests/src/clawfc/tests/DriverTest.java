/*
 * @author Mikhail Zhigun
 * @copyright Copyright 2020, MeteoSwiss
 */
package clawfc.tests;

import clawfc.Driver;
import junit.framework.TestCase;

public class DriverTest extends TestCase
{
    public void testRun() throws Exception
    {
        Driver.run(new String[0]);
    }
}
