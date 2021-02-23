/*
 * @author Mikhail Zhigun
 * @copyright Copyright 2021, MeteoSwiss
 */
package claw.tests;

import java.nio.file.Path;
import java.util.Arrays;

import claw.utils.BasicTestCase;

public class OpenACCTest extends BasicTestCase
{
    InputParams createParams(String name)
    {
        final String relpath = "openacc/" + name;
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

    public void test_array_simple_transformation_with_1_dimension() throws Exception
    {
        run("array_simple_transformation_with_1_dimension");
    }

    public void test_array_simple_transformation_with_2_dimension() throws Exception
    {
        run("array_simple_transformation_with_2_dimension");
    }

    public void test_array_block_transformation_without_dependency() throws Exception
    {
        run("array_block_transformation_without_dependency");
    }

    public void test_array_block_transformation_without_dependency_and_allocatable_output() throws Exception
    {
        InputParams inParams = createParams("array_block_transformation_without_dependency_and_allocatable_output");
        inParams.setCompare(true);
        run(inParams);
    }

    public void test_array_lower_upper_bounds_with_offset() throws Exception
    {
        run("array_lower_upper_bounds_with_offset");
    }

    public void test_array_induction_clause() throws Exception
    {
        run("array_induction_clause");
    }

    public void test_array_block_transformation_with_different_index_ranges() throws Exception
    {
        run("array_block_transformation_with_different_index_ranges");
    }

    public void test_array_with_target_clause() throws Exception
    {
        run("array_with_target_clause");
    }

    public void test_continuation() throws Exception
    {
        run("continuation");
    }

    public void test_lib() throws Exception
    {
        InputParams inParams = createParams("lib");
        inParams.setCompile(false);
        run(inParams);
    }

    public void test_primitive1() throws Exception
    {
        InputParams inParams = createParams("primitive1");
        inParams.setClawFlags(Arrays.asList("--target=gpu", "--directive=openacc"));
        run(inParams);
    }

    public void test_primitive2() throws Exception
    {
        InputParams inParams = createParams("primitive2");
        inParams.setClawFlags(Arrays.asList("--target=gpu", "--directive=openacc"));
        inParams.setCompile(false);
        run(inParams);
    }

    public void test_primitive3() throws Exception
    {
        InputParams inParams = createParams("primitive3");
        inParams.setClawFlags(Arrays.asList("--target=gpu", "--directive=openacc"));
        run(inParams);
    }

    public void test_primitive4() throws Exception
    {
        InputParams inParams = createParams("primitive4");
        inParams.setClawFlags(Arrays.asList("--target=gpu", "--directive=openacc"));
        run(inParams);
    }
}
