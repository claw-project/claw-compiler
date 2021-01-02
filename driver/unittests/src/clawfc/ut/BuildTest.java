/*
 * @author Mikhail Zhigun
 * @copyright Copyright 2020, MeteoSwiss
 */
package clawfc.ut;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import clawfc.Build;
import clawfc.BuildOrder;
import clawfc.FortranFileBuildInfoData;
import clawfc.ModuleInfo;
import clawfc.XmodData;
import clawfc.depscan.FortranDepScanner;
import clawfc.depscan.FortranFileBuildInfo;
import clawfc.depscan.FortranModuleInfo;
import clawfc.depscan.FortranSemanticException;
import clawfc.utils.AsciiArrayIOStream;
import clawfc.utils.FileInfo;
import junit.framework.TestCase;

class TestModuleInfo implements clawfc.ModuleInfo
{
    final String name;
    final FortranFileBuildInfo fileSrcInfo;
    final FortranModuleInfo moduleInfo;
    final Path filePath;
    final FileInfo fileInfo;

    public TestModuleInfo(String name, Path filePath, FortranFileBuildInfo fileInfo, FortranModuleInfo moduleInfo)
            throws Exception
    {
        this.name = name;
        this.fileSrcInfo = fileInfo;
        this.moduleInfo = moduleInfo;
        this.filePath = filePath;
        this.fileInfo = new clawfc.utils.FileInfoImpl(filePath);
    }

    @Override
    public String getName()
    {
        return name;
    }

    @Override
    public boolean isProgram()
    {
        throw new RuntimeException("Not implemented");
    }

    @Override
    public boolean isModule()
    {
        throw new RuntimeException("Not implemented");
    }

    @Override
    public boolean isInput()
    {
        throw new RuntimeException("Not implemented");
    }

    @Override
    public boolean usesCLAW()
    {
        return moduleInfo.getUsesClaw();
    }

    @Override
    public boolean hasSource()
    {
        return moduleInfo != null;
    }

    @Override
    public Collection<String> getUsedModules()
    {
        return moduleInfo.getUsedModuleNames();
    }

    @Override
    public FortranModuleInfo getModuleSrcInfo()
    {
        return moduleInfo;
    }

    @Override
    public AsciiArrayIOStream getPreprocessedSrc(boolean preserveOffset) throws IOException
    {
        throw new RuntimeException("Not implemented");
    }

    @Override
    public Path getSrcPath()
    {
        return this.filePath;
    }

    @Override
    public FortranFileBuildInfoData getSrcFileBinfoData()
    {
        throw new RuntimeException("Not implemented");
    }

    @Override
    public XmodData getXMod()
    {
        throw new RuntimeException("Not implemented");
    }

    @Override
    public Path getPPSrcPath()
    {
        throw new RuntimeException("Not implemented");
    }

    @Override
    public AsciiArrayIOStream getXast()
    {
        throw new RuntimeException("Not implemented");
    }

    @Override
    public AsciiArrayIOStream getTxast()
    {
        throw new RuntimeException("Not implemented");
    }

}

public class BuildTest extends TestCase
{
    protected final Path RES_DIR = clawfc.ut.Resources.DIR;

    Map<String, ModuleInfo> getModulesInfo(Path filePath) throws Exception
    {
        Map<String, ModuleInfo> res = new LinkedHashMap<String, ModuleInfo>();
        FortranDepScanner depScanner = new FortranDepScanner();
        FortranFileBuildInfo fileInfo = depScanner.scan(Files.newInputStream(filePath));
        for (FortranModuleInfo moduleInfo : fileInfo.getModules())
        {
            String name = moduleInfo.getName();
            res.put(name, new TestModuleInfo(name, filePath, fileInfo, moduleInfo));
        }
        if (fileInfo.getProgram() != null)
        {
            FortranModuleInfo moduleInfo = fileInfo.getProgram();
            String name = moduleInfo.getName();
            res.put(name, new TestModuleInfo(name, filePath, fileInfo, moduleInfo));
        }
        return res;
    }

    public void testSanityCheck() throws Exception
    {
        final Path IN_DIR = RES_DIR.resolve("build/sanity_check/input");
        final Path NORMAL_FILEPATH = IN_DIR.resolve("normal.f90");
        final Path MUL_TARGETS_FILEPATH = IN_DIR.resolve("mul_targets.f90");
        final Path UNDEFINED_MOD_FILEPATH = IN_DIR.resolve("undefined_module.f90");
        final Path CIRCLE_DEP_FILEPATH = IN_DIR.resolve("circle_dep.f90");
        {
            Map<String, ModuleInfo> modules = getModulesInfo(NORMAL_FILEPATH);
            Build.sanityCheck(modules, Stream.of("p1").collect(Collectors.toSet()));
        }
        {
            Map<String, ModuleInfo> modules = getModulesInfo(MUL_TARGETS_FILEPATH);
            Build.sanityCheck(modules, Stream.of("t1", "t2", "t3").collect(Collectors.toSet()));
        }
        {
            Map<String, ModuleInfo> modules = getModulesInfo(UNDEFINED_MOD_FILEPATH);
            boolean exCaught = false;
            try
            {
                Build.sanityCheck(modules, Stream.of("p1").collect(Collectors.toSet()));
            } catch (FortranSemanticException e)
            {
                String errMsg = e.getMessage();
                assertTrue(errMsg.contains("Module \"mod_undefined\" used by module p1"));
                assertTrue(
                        errMsg.contains("undefined_module.f90:12) is not defined in any file under given search path"));
                exCaught = true;
            }
            assertTrue(exCaught);
        }
        {
            Map<String, ModuleInfo> modules = getModulesInfo(CIRCLE_DEP_FILEPATH);
            boolean exCaught = false;
            try
            {
                Build.sanityCheck(modules, Stream.of("t").collect(Collectors.toSet()));
            } catch (FortranSemanticException e)
            {
                String errMsg = e.getMessage();
                assertTrue(errMsg.contains("Circle dependency between modules mod1"));
                assertTrue(errMsg.contains("circle_dep.f90:1) and t"));
                assertTrue(errMsg.contains("circle_dep.f90:13"));
                exCaught = true;
            }
            assertTrue(exCaught);
        }
    }

    public void testRemoveUnreferencedModules() throws Exception
    {
        final Path IN_DIR = RES_DIR.resolve("build/remove_unref_modules/input");
        final Path FILEPATH = IN_DIR.resolve("unref_modules.f90");
        Map<String, ModuleInfo> modules = getModulesInfo(FILEPATH);
        Map<String, ModuleInfo> res = Build.removeUnreferencedModules(modules,
                Stream.of("t").collect(Collectors.toSet()));
        assertEquals(Stream.of("mod1", "t").collect(Collectors.toSet()), res.keySet());
    }

    List<String> getBuildSeq(Map<String, ModuleInfo> modules, Set<String> targets)
    {
        List<String> res = new ArrayList<String>();
        BuildOrder order = Build.getParallelOrder(modules, targets);
        assertEquals(modules, order.getUsedModules());
        assertEquals(targets, order.getTargetModules());
        while (!order.done())
        {
            String next = order.next();
            res.add(next);
            order.onProcessed(next);
            assertTrue(order.getProcessedModules().contains(next));
        }
        return res;
    }

    void verifyParallelBuildOrder(Path filePath, List<String> targets, List<String> expBuildSeq) throws Exception
    {
        Map<String, ModuleInfo> modules = getModulesInfo(filePath);
        List<String> ordSeq = getBuildSeq(modules, Stream.of("t").collect(Collectors.toSet()));
        assertEquals(expBuildSeq, ordSeq);
    }

    public void testParallelBuildOrder() throws Exception
    {
        final Path IN_DIR = RES_DIR.resolve("build/parallel_order/input");
        final Path FILEPATH1 = IN_DIR.resolve("1.f90");
        final Path FILEPATH2 = IN_DIR.resolve("2.f90");
        final Path FILEPATH3 = IN_DIR.resolve("3.f90");
        verifyParallelBuildOrder(FILEPATH1, Arrays.asList("t"), Arrays.asList("m11", "m12", "m1", "m2", "m3", "t"));
        verifyParallelBuildOrder(FILEPATH2, Arrays.asList("t"), Arrays.asList("m11", "m12", "m2", "m3", "m1", "t"));
        verifyParallelBuildOrder(FILEPATH3, Arrays.asList("t", "t2", "t3"),
                Arrays.asList("m11", "m12", "m2", "m3", "m1", "t", "t2", "t3"));
    }
}
