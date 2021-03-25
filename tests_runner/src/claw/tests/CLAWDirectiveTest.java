/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 * @author Mikhail Zhigun
 */
package claw.tests;

import java.nio.file.Path;
import java.util.Arrays;

import claw.utils.BasicTestCase;

public class CLAWDirectiveTest extends BasicTestCase
{
    InputParams createParams(String name)
    {
        final String relpath = "claw/directive/" + name;
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

    public void test_array_access_to_function_call() throws Exception
    {
        run("array_access_to_function_call");
    }

    public void test_array_access_to_function_call_with_target_clause() throws Exception
    {
        run("array_access_to_function_call_with_target_clause");
    }

    public void test_compile_guard_openacc_with_claw_directives() throws Exception
    {
        InputParams inParams = createParams("compile_guard_openacc_with_claw_directives");
        inParams.setClawFlags(Arrays.asList("--directive=openacc"));
        inParams.setCompare(false);
        run(inParams);
    }

    public void test_compile_guard_openacc() throws Exception
    {
        run("compile_guard_openacc");
    }

    public void test_compile_guard_openmp_with_claw_directives() throws Exception
    {
        InputParams inParams = createParams("compile_guard_openmp_with_claw_directives");
        inParams.setClawFlags(Arrays.asList("--directive=openmp"));
        inParams.setCompare(false);
        run(inParams);
    }

    public void test_compile_guard_openmp() throws Exception
    {
        InputParams inParams = createParams("compile_guard_openmp");
        inParams.setClawFlags(Arrays.asList("--directive=openmp"));
        inParams.setCompare(false);
        run(inParams);
    }

    public void test_kcache() throws Exception
    {
        InputParams inParams = createParams("kcache");
        inParams.setCompare(false);
        run(inParams);
    }

    public void test_kcache_with_init() throws Exception
    {
        InputParams inParams = createParams("kcache_with_init");
        inParams.setCompare(false);
        run(inParams);
    }

    public void test_kcache_with_init_and_data() throws Exception
    {
        InputParams inParams = createParams("kcache_with_init_and_data");
        inParams.setCompare(false);
        run(inParams);
    }

    public void test_kcache_with_init_and_data_and_private() throws Exception
    {
        InputParams inParams = createParams("kcache_with_init_and_data_and_private");
        inParams.setCompare(false);
        run(inParams);
    }

    public void test_kcache_with_target() throws Exception
    {
        InputParams inParams = createParams("kcache_with_target");
        inParams.setCompare(false);
        run(inParams);
    }
}
