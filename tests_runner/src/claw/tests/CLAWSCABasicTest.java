/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 * @author Mikhail Zhigun
 */
package claw.tests;

import java.nio.file.Path;
import java.util.Arrays;

import claw.utils.BasicTestCase;

public class CLAWSCABasicTest extends BasicTestCase
{
    InputParams createParams(String name)
    {
        final String relpath = "claw/sca/" + name;
        final Path resDir = RES_DIR.resolve(relpath);
        final Path workingDir = WORKING_DIR;
        InputParams p = new InputParams(name, resDir, workingDir);
        p.setInputDirName("");
        p.setRefDirName("");
        p.setOutputDirName("");
        p.setCompare(true);
        return p;
    }

    void run(String name) throws Exception
    {
        InputParams inParams = createParams(name);
        run(inParams);
    }

    public void test_loop_insertion_issue_with_directive_none() throws Exception
    {
        InputParams inParams = createParams("loop_insertion_issue_with_directive_none");
        inParams.setClawFlags(Arrays.asList("--directive=none"));
        inParams.setCompare(false);
        inParams.setCompile(false);
        run(inParams);
    }
}
