/*
 * @author Mikhail Zhigun
 * @copyright Copyright 2021, MeteoSwiss
 */
package claw.tests;

import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import claw.utils.BasicTestCase;

public class DriverTest extends BasicTestCase
{
    InputParams createParams(String name)
    {
        final String relpath = "driver/" + name;
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

    void forceRun(String name, List<String> clawFlags) throws Exception
    {
        List<String> flags = new ArrayList<String>();
        flags.addAll(clawFlags);
        flags.add("--force");
        InputParams inParams = createParams(name);
        inParams.setClawFlags(flags);
        run(inParams);
    }

    void forceRun(String name) throws Exception
    {
        forceRun(name, Arrays.asList());
    }

    public void test_add_paren() throws Exception
    {
        forceRun("add_paren", Arrays.asList("--add-paren"));
    }

    public void test_backslash_preprocessor() throws Exception
    {
        InputParams inParams = createParams("backslash_preprocessor");
        inParams.setClawFlags(Arrays.asList("--force"));
        inParams.setOriginal("original_code.f90");
        inParams.setCompile(false);
        run(inParams);
    }

    public void test_multiple_dependencies_with_subfolder() throws Exception
    {
        InputParams inParams = createParams("multiple_dependencies_with_subfolder");
        inParams.setRefDirName("reference");
        inParams.setClawFlags(Arrays.asList("--force", "-SI", "subfolder", "-SI", ".", "-D__DEP2__", "--verbose"));
        inParams.setCompile(false);
        run(inParams);
    }

    public void test_one_dependency_in_a_subfolder() throws Exception
    {
        InputParams inParams = createParams("one_dependency_in_a_subfolder");
        inParams.setRefDirName("reference");
        inParams.setClawFlags(Arrays.asList("--force", "-SI", "subfolder"));
        inParams.setCompile(false);
        run(inParams);
    }

    public void test_dependencies3() throws Exception
    {
        InputParams inParams = createParams("dependencies3");
        inParams.setRefDirName("reference");
        inParams.setClawFlags(Arrays.asList("--force", "-SI", "subfolder", "-SI", "."));
        inParams.setCompile(false);
        run(inParams);
    }

    public void test_dependencies4() throws Exception
    {
        InputParams inParams = createParams("dependencies4");
        inParams.setRefDirName("reference");
        inParams.setClawFlags(Arrays.asList("--force", "-MI", "."));
        inParams.setCompile(false);
        run(inParams);
    }

    public void test_dependencies5() throws Exception
    {
        InputParams inParams = createParams("dependencies5");
        inParams.setRefDirName("reference");
        inParams.setClawFlags(Arrays.asList("--force", "-SI", "."));
        inParams.setCompile(false);
        run(inParams);
    }

    public void test_intrinsic_module_dependency_iso_c_binding() throws Exception
    {
        forceRun("intrinsic_module_dependency_iso_c_binding", Arrays.asList());
    }

    public void test_intrinsic_module_dependency_iso_fortran_env() throws Exception
    {
        forceRun("intrinsic_module_dependency_iso_fortran_env", Arrays.asList());
    }

    public void test_intrinsic_module_dependency_ieee() throws Exception
    {
        forceRun("intrinsic_module_dependency_ieee", Arrays.asList());
    }

    public void test_force() throws Exception
    {
        forceRun("force", Arrays.asList());
    }

    public void test_ignore1() throws Exception
    {
        InputParams inParams = createParams("ignore1");
        inParams.setCompare(false);
        run(inParams);
    }

    public void test_ignore2() throws Exception
    {
        InputParams inParams = createParams("ignore2");
        inParams.setCompare(false);
        run(inParams);
    }

    public void test_ignore3() throws Exception
    {
        InputParams inParams = createParams("ignore3");
        inParams.setCompare(false);
        run(inParams);
    }

    public void test_automatic_openacc_macro() throws Exception
    {
        InputParams inParams = createParams("automatic_openacc_macro");
        inParams.setOriginal("original_code.F90");
        inParams.setClawFlags(Arrays.asList("--force", "--target=gpu", "--directive=openacc"));
        inParams.setCompare(false);
        run(inParams);
    }

    public void test_automatic_openmp_macro() throws Exception
    {
        InputParams inParams = createParams("automatic_openmp_macro");
        inParams.setOriginal("original_code.F90");
        inParams.setClawFlags(Arrays.asList("--force", "--directive=openmp"));
        inParams.setCompare(false);
        run(inParams);
    }

    public void test_user_defined_macro() throws Exception
    {
        InputParams inParams = createParams("user_defined_macro");
        inParams.setOriginal("original_code.F90");
        inParams.setClawFlags(Arrays.asList("--force", "-DUSER_MACRO1", "-DUSER_MACRO2"));
        inParams.setCompare(false);
        run(inParams);
    }

    public void test_fusion_with_openacc_macro() throws Exception
    {
        InputParams inParams = createParams("fusion_with_openacc_macro");
        inParams.setOriginal("original_code.F90");
        inParams.setClawFlags(Arrays.asList("--force", "--target=gpu", "--directive=openacc"));
        inParams.setCompare(false);
        run(inParams);
    }

    public void test_fusion_with_openmp_macro() throws Exception
    {
        InputParams inParams = createParams("fusion_with_openmp_macro");
        inParams.setOriginal("original_code.F90");
        inParams.setClawFlags(Arrays.asList("--force", "--target=gpu", "--directive=openmp"));
        inParams.setCompare(false);
        run(inParams);
    }

    public void test_verbatim_directive() throws Exception
    {
        InputParams inParams = createParams("verbatim_directive");
        inParams.setCompare(false);
        run(inParams);
    }

    public void test_external_transformation_set() throws Exception
    {
        InputParams inParams = createParams("external_transformation_set");
        inParams.setClawFlags(Arrays.asList("--force", "--config=external_config.xml"));
        final Path extSetDirPath = Resources.INT_INSTALL_DIR.resolve("share/claw");
        inParams.setClawTransSetPath(extSetDirPath);
        inParams.setCompile(false);
        run(inParams);
    }

    public void test_keep_comment() throws Exception
    {
        forceRun("keep_comment", Arrays.asList("--keep-comment"));
    }
}
