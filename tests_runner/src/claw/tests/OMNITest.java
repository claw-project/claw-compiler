/*
 * @author Mikhail Zhigun
 * @copyright Copyright 2021, MeteoSwiss
 */
package claw.tests;

import java.nio.file.Path;
import java.util.Arrays;

import claw.utils.AdvancedTestCase;

public class OMNITest extends AdvancedTestCase
{
    InputParams createParams(String name)
    {
        final String relpath = "omni/" + name;
        final Path resDir = RES_DIR.resolve(relpath);
        final Path workingDir = WORKING_DIR.resolve(relpath);
        InputParams p = new InputParams(name, resDir, workingDir);
        p.setInputDirName("");
        p.setRefDirName("");
        p.setOutputDirName("");
        p.setCompare(true);
        p.setDebugClawfc(false);
        p.setLink(false);
        return p;
    }

    void run(String name) throws Exception
    {
        InputParams inParams = createParams(name);
        run(inParams);
    }

    public void test_constant_kind() throws Exception
    {
        InputParams inParams = createParams("constant_kind");
        inParams.setClawFlags(Arrays.asList("--directive=none", "--force"));
        inParams.setCompare(false);
        run(inParams);
    }
}
