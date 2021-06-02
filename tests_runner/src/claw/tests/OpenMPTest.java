/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 * @author Mikhail Zhigun
 */
package claw.tests;

import java.nio.file.Path;
import java.util.Arrays;

import claw.utils.BasicTestCase;

public class OpenMPTest extends BasicTestCase
{
    InputParams createParams(String name)
    {
        final String relpath = "openmp/" + name;
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

    public void test_primitive1() throws Exception
    {
        InputParams inParams = createParams("primitive1");
        inParams.setClawFlags(Arrays.asList("--target=gpu", "--directive=openmp"));
        run(inParams);
    }
}
