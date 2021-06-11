/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 * @author Mikhail Zhigun
 */
package claw.tests;

import java.nio.file.Path;
import java.util.Arrays;

import claw.utils.AdvancedTestCase;

public class CLAWSingleColumnAbstractionTest extends AdvancedTestCase
{
    InputParams createParams(String name)
    {
        final String relpath = "claw/sca/" + name;
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

    public void test_simple_1d() throws Exception
    {
        run("simple_1d");
    }

    public void test_simple_2d() throws Exception
    {
        run("simple_2d");
    }

    public void test_openmp() throws Exception
    {
        InputParams inParams = createParams("openmp");
        inParams.setUseCPUOpenMPTarget(true);
        run(inParams);
    }

    public void test_simple_with_forward() throws Exception
    {
        run("simple_with_forward");
    }

    public void test_cross_module() throws Exception
    {
        run("cross-module");
    }

    public void test_cross_module_multiple_forward() throws Exception
    {
        run("cross-module_multiple_forward");
    }

    public void test_forward_with_function_result() throws Exception
    {
        run("forward_with_function_result");
    }

    public void test_forward_dependencies() throws Exception
    {
        run("forward_dependencies");
    }

    public void test_private_function() throws Exception
    {
        run("private_function");
    }

    public void test_private_function_fct_reorder() throws Exception
    {
        run("private_function_fct_reorder");
    }

    public void test_data_over_with_assumed_shaped_arrays() throws Exception
    {
        run("data_over_with_assumed_shaped_arrays");
    }

    public void test_data_over_with_non_assumed_shaped_arrays() throws Exception
    {
        run("data_over_with_non_assumed_shaped_arrays");
    }

    public void test_data_over_with_non_assumed_shaped_arrays_and_multiple_over_clauses() throws Exception
    {
        run("data_over_with_non_assumed_shaped_arrays_and_multiple_over_clauses");
    }

    public void test_adding_dimension_in_the_middle_of_already_defined_ones() throws Exception
    {
        run("adding_dimension_in_the_middle_of_already_defined_ones");
    }

    public void test_data_over_clause_on_a_scalar() throws Exception
    {
        run("data_over_clause_on_a_scalar");
    }

    public void test_sca_forward_over_type_bound_procedure_call() throws Exception
    {
        run("sca_forward_over_type_bound_procedure_call");
    }

    public void test_sca_in_a_subroutine_with_contains() throws Exception
    {
        run("sca_in_a_subroutine_with_contains");
    }

    public void test_sca_region_start() throws Exception
    {
        run("sca_region_start");
    }

    public void test_sca_with_automatic_promotion_std() throws Exception
    {
        run("sca_with_automatic_promotion_std");
    }

    public void test_sca_in_a_subroutine_with_contains2() throws Exception
    {
        run("sca_in_a_subroutine_with_contains2");
    }

    public void test_sca_forward_with_optional_not_set() throws Exception
    {
        run("sca_forward_with_optional_not_set");
    }

    public void test_sca_forward_with_optional_set() throws Exception
    {
        run("sca_forward_with_optional_set");
    }

    public void test_sca_with_automatic_promotion_placed() throws Exception
    {
        run("sca_with_automatic_promotion_placed");
    }

    public void test_sca_with_promotion_of_result_var() throws Exception
    {
        run("sca_with_promotion_of_result_var");
    }

    public void test_sca_forward_with_namedvalue() throws Exception
    {
        run("sca_forward_with_namedvalue");
    }

    public void test_sca_forward_non_fct_field() throws Exception
    {
        run("sca_forward_non_fct_field");
    }

    public void test_sca_forward_with_scalar_to_1d_propagation() throws Exception
    {
        run("sca_forward_with_scalar_to_1d_propagation");
    }

    public void test_pure_keyword() throws Exception
    {
        run("pure_keyword");
    }

    public void test_sca28() throws Exception
    {
        run("sca28");
    }

    public void test_claw_nodep_on_k_loop_openacc_collapse_eq_false() throws Exception
    {
        run("claw_nodep_on_k_loop_openacc_collapse_eq_false");
    }

    public void test_sca_forward_with_type_member_slicing() throws Exception
    {
        InputParams inParams = createParams("sca_forward_with_type_member_slicing");
        inParams.setCompile(false);
        run(inParams);
    }

    public void test_promotion_instead_of_privatization_of_values_and_parallel_region_start_end_after_before_unsupported_and_update_create()
            throws Exception
    {
        InputParams inParams = createParams(
                "promotion_instead_of_privatization_of_values_and_parallel_region_start_end_after_before_unsupported_and_update_create");
        inParams.setConfigFilename("promote.xml");
        run(inParams);
    }

    public void test_claw_nodep_on_k_loop_openacc_collapse_eq_true() throws Exception
    {
        InputParams inParams = createParams("claw_nodep_on_k_loop_openacc_collapse_eq_true");
        inParams.setConfigFilename("enable_collapse.xml");
        run(inParams);
    }

    public void test_cpu_specific_categorization_for_assign_statement() throws Exception
    {
        run("cpu_specific_categorization_for_assign_statement");
    }

    public void test_cpu_handle_if_correcly() throws Exception
    {
        run("cpu_handle_if_correcly");
    }

    public void test_correct_handling_of_indirect_promotion() throws Exception
    {
        run("correct_handling_of_indirect_promotion");
    }

    public void test_check_not_mixed_acc_loop_seq_order_directive_generation() throws Exception
    {
        run("check_not_mixed_acc_loop_seq_order_directive_generation");
    }

    public void test_check_independent_if_handling_for_cpu_target() throws Exception
    {
        run("check_independent_if_handling_for_cpu_target");
    }

    public void test_same_as_sca1_with_override_option_passed_to_the_driver_x() throws Exception
    {
        InputParams inParams = createParams("same_as_sca1_with_override_option_passed_to_the_driver_x");
        inParams.setClawFlags(Arrays.asList("-x=cpu_trans_strategy:single"));
        run(inParams);
    }

    public void test_sca_with_model_config() throws Exception
    {
        InputParams inParams = createParams("sca_with_model_config");
        inParams.setModelConfigFilename("basic_model.toml");
        run(inParams);
    }

    public void test_sca_with_model_config_and_separate_size_and_iteration_dimension_variables() throws Exception
    {
        InputParams inParams = createParams(
                "sca_with_model_config_and_separate_size_and_iteration_dimension_variables");
        inParams.setModelConfigFilename("iteration_size_model.toml");
        run(inParams);
    }

    public void test_sca_on_elemental_function() throws Exception
    {
        InputParams inParams = createParams("sca_on_elemental_function");
        inParams.setModelConfigFilename("model.toml");
        inParams.setClawFlags(Arrays.asList("-x=sca_elemental_promotion_assumed:false"));
        run(inParams);
    }

    public void test_bug_for_499() throws Exception
    {
        run("bug_for_499");
    }

    public void test_sca_on_elemental_function_with_present_intrinsic() throws Exception // 43
    {
        InputParams inParams = createParams("sca_on_elemental_function_with_present_intrinsic");
        inParams.setModelConfigFilename("model.toml");
        inParams.setClawFlags(Arrays.asList("-x=sca_elemental_promotion_assumed:false"));
        run(inParams);
    }

    public void test_sca_routine() throws Exception // 45
    {
        InputParams inParams = createParams("sca_routine");
        inParams.setModelConfigFilename("model.toml");
        inParams.setClawFlags(Arrays.asList("-x=sca_elemental_promotion_assumed:false"));
        // TODO: find out what's wrong with OpenMP file
        inParams.setCompile(false);
        run(inParams);
    }

    public void test_simple_return_transformation() throws Exception
    {
        run("simple_return_transformation");
    }

    public void test_issue_578_regression() throws Exception
    {
        InputParams inParams = createParams("issue_578_regression");
        // TODO: find out what's wrong with OpenMP file
        inParams.setCompile(false);
        run(inParams);
    }
}
