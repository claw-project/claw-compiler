/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 * @author Mikhail Zhigun
 */
package claw.tests;

import java.nio.file.Path;

import claw.utils.BasicTestCase;

public class UtilitiesTest extends BasicTestCase
{
    InputParams createParams(String name)
    {
        final String relpath = "utilities/" + name;
        final Path resDir = RES_DIR.resolve(relpath);
        final Path workingDir = WORKING_DIR.resolve(relpath);
        InputParams p = new InputParams(name, resDir, workingDir);
        p.setInputDirName("");
        p.setRefDirName("");
        p.setOutputDirName("");
        p.setCompare(false);
        p.setDebugClawfc(false);
        return p;
    }

    void run(String name) throws Exception
    {
        InputParams inParams = createParams(name);
        run(inParams);
    }

    public void test_remove1() throws Exception
    {
        run("remove1");
    }

    public void test_remove2() throws Exception
    {
        run("remove2");
    }

    public void test_remove3() throws Exception
    {
        run("remove3");
    }

    public void test_remove4() throws Exception
    {
        run("remove4");
    }
}
