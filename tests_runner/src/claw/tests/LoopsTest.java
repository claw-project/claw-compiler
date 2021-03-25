/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 * @author Mikhail Zhigun
 */
package claw.tests;

import java.nio.file.Path;
import java.util.Arrays;

import claw.utils.BasicTestCase;

public class LoopsTest extends BasicTestCase
{
    InputParams createParams(String name)
    {
        final String relpath = "loops/" + name;
        final Path resDir = RES_DIR.resolve(relpath);
        final Path workingDir = WORKING_DIR.resolve(relpath);
        InputParams p = new InputParams(name, resDir, workingDir);
        p.setInputDirName("");
        p.setRefDirName("");
        p.setOutputDirName("");
        p.setCompare(true);
        p.setDebugClawfc(false);
        return p;
    }

    void run(String name) throws Exception
    {
        InputParams inParams = createParams(name);
        run(inParams);
    }

    public void test_extract1() throws Exception
    {
        run("extract1");
    }

    public void test_extract2() throws Exception
    {
        run("extract2");
    }

    public void test_extract3() throws Exception
    {
        run("extract3");
    }

    public void test_extract4() throws Exception
    {
        run("extract4");
    }

    public void test_extract5() throws Exception
    {
        InputParams inParams = createParams("extract5");
        inParams.setCompare(false);
        run(inParams);
    }

    public void test_extract6() throws Exception
    {
        InputParams inParams = createParams("extract6");
        inParams.setCompare(false);
        run(inParams);
    }

    public void test_extract_with_target_clause() throws Exception
    {
        run("extract_with_target_clause");
    }

    public void test_fusion1() throws Exception
    {
        InputParams inParams = createParams("fusion1");
        inParams.setCompare(false);
        run(inParams);
    }

    public void test_fusion2() throws Exception
    {
        InputParams inParams = createParams("fusion2");
        inParams.setCompare(false);
        run(inParams);
    }

    public void test_fusion3() throws Exception
    {
        InputParams inParams = createParams("fusion3");
        inParams.setCompare(false);
        run(inParams);
    }

    public void test_fusion4() throws Exception
    {
        InputParams inParams = createParams("fusion4");
        inParams.setCompare(false);
        run(inParams);
    }

    public void test_fusion5() throws Exception
    {
        InputParams inParams = createParams("fusion5");
        inParams.setCompare(false);
        run(inParams);
    }

    public void test_fusion6() throws Exception
    {
        InputParams inParams = createParams("fusion6");
        inParams.setCompare(false);
        run(inParams);
    }

    public void test_fusion_with_target_clause() throws Exception
    {
        InputParams inParams = createParams("fusion_with_target_clause");
        inParams.setCompare(false);
        run(inParams);
    }

    public void test_impossible_fusion_because_of_assignment() throws Exception
    {
        InputParams inParams = createParams("impossible_fusion_because_of_assignment");
        inParams.setCompare(false);
        run(inParams);
    }

    public void test_fusion_issue121_release_constraint() throws Exception
    {
        InputParams inParams = createParams("fusion_issue121_release_constraint");
        inParams.setCompare(false);
        run(inParams);
    }

    public void test_fusion10_issue_121_and_hoist() throws Exception
    {
        InputParams inParams = createParams("fusion10_issue_121_and_hoist");
        inParams.setCompare(false);
        inParams.setClawFlags(Arrays.asList("--config=fusion_first.xml"));
        run(inParams);
    }

    public void test_fusion11_test_case_for_ecmwf_cloudsc() throws Exception
    {
        InputParams inParams = createParams("fusion11_test_case_for_ecmwf_cloudsc");
        inParams.setCompare(false);
        inParams.setClawFlags(Arrays.asList("--config=dedicated_config.xml"));
        run(inParams);
    }

    public void test_fusion_behavior_with_openmp_directive() throws Exception
    {
        InputParams inParams = createParams("fusion_behavior_with_openmp_directive");
        inParams.setCompare(false);
        inParams.setClawFlags(Arrays.asList("--directive=openmp"));
        run(inParams);
    }

    public void test_hoist1() throws Exception
    {
        InputParams inParams = createParams("hoist1");
        inParams.setCompare(false);
        run(inParams);
    }

    public void test_hoist2() throws Exception
    {
        InputParams inParams = createParams("hoist2");
        inParams.setCompare(false);
        run(inParams);
    }

    public void test_hoist_with_target_clause() throws Exception
    {
        InputParams inParams = createParams("hoist_with_target_clause");
        inParams.setCompare(false);
        run(inParams);
    }

    public void test_hoist_issue_122_nested_in_do_stmt() throws Exception
    {
        InputParams inParams = createParams("hoist_issue_122_nested_in_do_stmt");
        inParams.setCompare(false);
        run(inParams);
    }

    public void test_hoist_issue_122_and_interchange_test() throws Exception
    {
        InputParams inParams = createParams("hoist_issue_122_and_interchange_test");
        inParams.setCompare(false);
        run(inParams);
    }

    public void test_hoist_with_fusion_group_clauses() throws Exception
    {
        InputParams inParams = createParams("hoist_with_fusion_group_clauses");
        inParams.setCompare(false);
        run(inParams);
    }

    public void test_hoist_with_fusion_group_collapse_clauses() throws Exception
    {
        InputParams inParams = createParams("hoist_with_fusion_group_collapse_clauses");
        inParams.setCompare(false);
        run(inParams);
    }

    public void test_hoist_cleanup_clause_for_both_openacc_and_openmp() throws Exception
    {
        InputParams inParams = createParams("hoist_cleanup_clause_for_both_openacc_and_openmp");
        inParams.setCompare(false);
        run(inParams);
    }

    public void test_hoist_cleanup_clause_for_openacc_only() throws Exception
    {
        InputParams inParams = createParams("hoist_cleanup_clause_for_openacc_only");
        inParams.setCompare(false);
        run(inParams);
    }

    public void test_hoist_cleanup_clause_for_openmp_only() throws Exception
    {
        InputParams inParams = createParams("hoist_cleanup_clause_for_openmp_only");
        inParams.setCompare(false);
        run(inParams);
    }

    public void test_nested_hoist() throws Exception
    {
        InputParams inParams = createParams("nested_hoist");
        inParams.setCompare(false);
        run(inParams);
    }

    public void test_extract_if_then_block() throws Exception
    {
        run("extract_if_then_block");
    }

    public void test_extract_if_then_else_block() throws Exception
    {
        run("extract_if_then_else_block");
    }

    public void test_interchange1() throws Exception
    {
        InputParams inParams = createParams("interchange1");
        inParams.setCompare(false);
        run(inParams);
    }

    public void test_interchange2() throws Exception
    {
        InputParams inParams = createParams("interchange2");
        inParams.setCompare(false);
        run(inParams);
    }

    public void test_interchange_with_target_clause() throws Exception
    {
        InputParams inParams = createParams("interchange_with_target_clause");
        inParams.setCompare(false);
        run(inParams);
    }
}
