/*
 * @author Mikhail Zhigun
 * @copyright Copyright 2020, MeteoSwiss
 */
package clawfc.tests;

import org.junit.runner.JUnitCore;
import org.junit.runner.Result;
import org.junit.runner.notification.Failure;

public class Main
{
   public static void main(String[] args)
   {
      Result result = JUnitCore.runClasses(DriverTest.class);

      for (Failure failure : result.getFailures())
      {
         System.out.println(failure.toString());
      }

      System.out.println(result.wasSuccessful());

      if(result.getFailures().isEmpty())
      { System.exit(0); }
      else
      { System.exit(1); }
   }
}
