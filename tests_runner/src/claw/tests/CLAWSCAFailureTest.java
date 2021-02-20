/*
 * @author Mikhail Zhigun
 * @copyright Copyright 2021, MeteoSwiss
 */
package claw.tests;

import java.nio.file.Path;

import claw.utils.FailureTestCase;

public class CLAWSCAFailureTest extends FailureTestCase
{
    InputParams createParams(String name, Target target, Directive directive, String expectedErrMsg)
    {
        final String relpath = "claw/sca/" + name;
        final Path resDir = RES_DIR.resolve(relpath);
        final Path workingDir = WORKING_DIR.resolve(relpath);
        InputParams p = new InputParams(name, resDir, workingDir, target, directive, expectedErrMsg);
        p.setInputDirName("");
        p.setOutputDirName("");
        p.setDebugClawfc(false);
        return p;
    }

    void run(String name, String expectedErrMsg) throws Exception
    {
        InputParams inParams = createParams(name, Target.GPU, Directive.OPENACC, expectedErrMsg);
        run(inParams);
    }

    public void test_unsupported_statement() throws Exception
    {
        run("unsupported_statement", "Unsupported statement in parallel region");
    }

    public void test_unsupported_statement_goto() throws Exception
    {
        run("unsupported_statement_goto", "Unsupported statement in parallel region: GOTO");
    }
}
